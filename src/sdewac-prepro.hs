-- sdewac-prepro.hs
-- (c) 2015 Jan Snajder
-- 
-- Usage: sdewac-prepro <lemmas> <dict> <corpus> 
-- Output: <lemma> <cpostag> <flag>
--         where flag indicates what change has been made:
--         P -- PTKVZ prefixation
--         H -- dehyphenation
--         B -- lemma backoff
--
-------------------------------------------------------------------------------

import ConllReader
import Control.Applicative
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.ParseArgs
import System.Exit
import System.IO

type LemmaList = Set Text
type LemmaDict = Map Text Text

data Flag = P | H | B | R deriving Show
type TokenExt = (Token, Maybe Flag)

readLemmaList :: FilePath -> IO LemmaList
readLemmaList f = 
  S.fromList . map (head . T.words) . T.lines <$> T.readFile f

inList :: LemmaList -> LemmaPos -> Bool
inList ls (l,p) = T.pack (l ++ posSep ++ p) `S.member` ls

-- PTKVZ (abgetrennter Verbzusatz)
ptkvz = "PTKVZ"

prefixPTKVZ :: LemmaList -> Sentence -> [TokenExt]
prefixPTKVZ ls s = map prefix ts
  where 
    ts = sentenceTokens s
    ps = M.fromList . map (\t -> (dephead t, t)) $
         filter ((==ptkvz) . postag) ts
    prefix t | cpostag t == "V" = 
                 case M.lookup (ix t) ps of
                   Just p  -> let ls' = [l' | p <- lemma p, l <- lemma t,
                                         let l' = p++l, inList ls (l',cpostag t) ]
                              in if null ls' then (t,Nothing)
                                 else (t { lemma = ls'}, Just P)
                   Nothing -> (t,Nothing)
             | otherwise = (t,Nothing)

-- dehyphenation
dehyphenate :: LemmaList -> TokenExt -> TokenExt
dehyphenate ls te@(t,_)
  | changed   = (t',Just H)
  | otherwise = te
  where t'    = t { lemma = [ deh l | l <- lemma t ] }
        deh l = let l' = removeHyphen l
                in if '-' `elem` l && inList ls (l',cpostag t) then l' else l
        changed = or $ zipWith (/=) (lemma t) (lemma t') 

removeHyphen :: String -> String
removeHyphen []     = []
removeHyphen (x:xs) = x : filter (/='-') (map toLower xs)

-- lemmatization backoff
readLemmaDict :: FilePath -> IO LemmaDict
readLemmaDict f = M.fromList . map parse . T.lines <$> T.readFile f
  where parse s = case T.words s of
          (w:l:_) -> (w,l)
          _       -> error "no parse"

lemmatize :: LemmaDict -> TokenExt -> TokenExt
lemmatize d te@(t,_) 
  | unknownLemma t && lemmaCh && posCh = (t',Just R)
  | unknownLemma t && lemmaCh          = (t',Just B)
  | otherwise                          = te
  where t' = case M.lookup wordPos d of
               Just l' -> let (l,p) = lemmaPos l' in 
                          t {lemma = [l], cpostag = p}
               Nothing -> t
        lemmaCh = lemma t' /= lemma t
        posCh   = cpostag t' /= cpostag t
        wordPos = T.pack $ form t ++ posSep ++ cpostag t
        lemmaPos l = case break (=='_') $ T.unpack l of
                       (l,_:p) -> (l,p)

preprocess :: LemmaList -> LemmaDict -> Corpus -> [[TokenExt]]
preprocess ls d = map (map (lemmatize d . dehyphenate ls) . prefixPTKVZ ls)

showTokenExt :: TokenExt -> String
showTokenExt (t,f) = intercalate "\t" $ 
  [intercalate "|" $ lemma t, cpostag t] ++ case f of
    Just f -> [show f]
    Nothing -> []

arg = 
  [ Arg 0 Nothing Nothing  (argDataRequired "lemmas" ArgtypeString)
      "lemma-POS list file"
  , Arg 1 Nothing Nothing  (argDataRequired "dict" ArgtypeString)
      "lemmatization dictionary file"
  , Arg 2 Nothing Nothing  (argDataRequired "corpus" ArgtypeString)
      "corpus in CoNLL format" ]

main :: IO ()
main = do
  args <- parseArgsIO ArgsComplete arg
  if not $ all (gotArg args) [0..2] then
    usageError args "Missing input files."
    exitFailure
  else do
    ls <- readLemmaList . fromJust $ getArg args 0
    d  <- readLemmaDict . fromJust $ getArg args 1
    c  <- readCorpus . fromJust $ getArg args 2
    putStr . unlines . map (unlines . map showTokenExt) $ preprocess ls d c
  hFlush stdout

