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

{-# LANGUAGE TupleSections, OverloadedStrings #-}

import ConllReader
import Control.Applicative
import Data.Char
import Data.Function (on)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Control.Monad (msum)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.ParseArgs
import System.Exit
import System.IO

type LemmaList = Map Text Int
type LemmaDict = Map (Text,Text) (Text,Text)

data Flag = P | H | Hp | B | Bp | Bc | X deriving Show
type TokenExt = (Token, Flag)

readLemmaList :: FilePath -> IO LemmaList
readLemmaList f = 
  M.fromList . map (parse . words) . lines <$> readFile f
  where parse (l:f:_) = (T.pack l,read f)
        parse _       = error "cannot parse"

inList :: LemmaList -> LemmaPos -> Bool
inList ls (l,p) = T.pack (l ++ posSep ++ p) `M.member` ls

lemmaFreq :: LemmaList -> LemmaPos -> Int
lemmaFreq ls (l,p) = M.findWithDefault 0 (T.pack $ l ++ posSep ++ p) ls

-- True if first lemma_POS is more frequent than the second lemma_POS
moreFrequent :: LemmaList -> LemmaPos -> LemmaPos -> LemmaPos
moreFrequent ls l1 l2 = 
  if lemmaFreq ls l1 > lemmaFreq ls l2 then l1 else l2
  
poses = map (:[]) "ACFKNPTV"

-- Returns the most frequent lemma_POS for a given lemma
mostFrequentPos :: LemmaList -> String -> Maybe String
mostFrequentPos ls l 
  | lemmaFreq ls (l,p) > 0 = Just p
  | otherwise              = Nothing
  where lx = map (l,) poses
        p  = snd $ maximumBy (compare `on` lemmaFreq ls) lx

-------------------------------------------------------------------------------
-- PTKVZ (abgetrennter Verbzusatz) prefixation
-------------------------------------------------------------------------------

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
                              in if null ls' then (t, X)
                                 else (t { lemma = ls'}, P)
                   Nothing -> (t, X)
             | otherwise = (t, X)

-------------------------------------------------------------------------------
-- Dehyphenation
-------------------------------------------------------------------------------

dehyphenate :: LemmaList -> TokenExt -> TokenExt
dehyphenate ls te@(t,_)
  | lemma t /= [] && lemmaCh = (t', if posCh then Hp else H)
  | otherwise = te
  where lx@((_,p'):_) = map (\l -> deh ls (l,cpostag t)) $ lemma t
        l' = map fst lx
        t' = t { lemma = l', cpostag = p' }
        lemmaCh = lemma t /= lemma t'
        posCh   = cpostag t /= cpostag t'
 
deh :: LemmaList -> LemmaPos -> LemmaPos
deh ls (l,p) | l /= l' && p == "T" && isJust p' = (l',fromJust p')
             | l /= l'             = moreFrequent ls (l',p) (l,p) 
             | otherwise           = (l,p)
  where l' = removeHyphen l
        p' = mostFrequentPos ls l'

removeHyphen :: String -> String
removeHyphen l | '-' `elem` l = remove l
               | otherwise    = l
  where remove []     = []
        remove (x:xs) = x : filter (/='-') (map toLower xs)

-------------------------------------------------------------------------------
-- Lemmatization backoff
-------------------------------------------------------------------------------

-- Reads in the lemmatization dictionary. Ignores lemmas that are 
-- "not good", as defined by the predicate below. Also removes the 
-- initial hyphens from lemmas.
readLemmaDict :: FilePath -> IO LemmaDict
readLemmaDict f = 
  M.fromList . filter (goodLP . snd) . map parse . T.lines <$> T.readFile f
  where parse s = case T.split (`elem` "\t_") s of
          (w:p1:l:p2:_) -> ((w,p1),(removeInitHyphen l,p2))
          _             -> error "no parse"

goodLP :: (Text,Text) -> Bool
goodLP (l,p) =
  ((p/="V") || (lowercased && any (`T.isSuffixOf` l) ["en","eln","ern"])) &&
  ((p/="A") || lowercased) &&
  (T.length l > 1)
  where lowercased = isLower $ T.head l

removeInitHyphen :: Text -> Text
removeInitHyphen l | T.head l == '-' = T.tail l
                   | otherwise       = l

type WordPos = LemmaPos

lemmaDictLookup :: LemmaDict -> WordPos -> Maybe LemmaPos
lemmaDictLookup d (w,p) = case M.lookup (T.pack w, T.pack p) d of
  Just (l,p) -> Just (T.unpack l, T.unpack p)
  Nothing    -> Nothing

-- try to back off to lemmatization dictiornay: 
-- first try original wordform_POS, then try other
-- poses, and if that doesn't work, try uppercased wordform with the
-- original POS
lemmaBackoff :: LemmaDict -> WordPos -> Maybe LemmaPos
lemmaBackoff d (w,p) = msum $ map (lemmaDictLookup d) wx
  where wx = (w,p) : map (w,) poses ++ [(w',p)]
        w' = toUpper (head w) : tail w

lemmatize :: LemmaDict -> TokenExt -> TokenExt
lemmatize d te@(t,_) 
  | unknownLemma t && lemmaCh = 
      (t', if posCh then Bp else if caseCh then Bc else B)
  | otherwise = te
  where t' = case lemmaBackoff d (form t, cpostag t) of
               Just (l',p') -> t {lemma = [l'], cpostag = p'}
               Nothing      -> t
        lemmaCh = lemma t /= lemma t'
        posCh   = cpostag t /= cpostag t'
        caseCh  = head (form t) /= head (head (lemma t'))

preprocess :: LemmaList -> LemmaDict -> Corpus -> [[TokenExt]]
preprocess ls d = map (map (lemmatize d . dehyphenate ls) . prefixPTKVZ ls)

showTokenExt :: TokenExt -> String
showTokenExt (t,f) = intercalate "\t" $ 
  [if null (lemma t) then "<unknown>" else intercalate "|" $ lemma t, 
   cpostag t, show f]

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

