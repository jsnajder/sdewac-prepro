-- sdewac-mstparsed2lemmas.hs
-- Counts lemmas in MST/TT-parsed sDeWaC
-- (c) 2013 Jan Snajder

import ConllReader
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

type LemmaList = Set Text
type LemmaDict = Map Text Text

{-
output: five columns:
 wordform lemma POS gender backoff-flag

(1) Construct lemma list from MST-parsed corpus
(2) Construct backoff dictionary from MATE-parsed corpus:
    wordform+POS=>lemma+POS
(3) Foreach MST sentence:
    (3a) prefix PTKVZ if resulting prefix+verb exists in list from (1)
    (3b) convert hyphenated lemmas to non-hyphenated variants, if such exist in (1)
    (3c) if lemma==unknown, attempt backoff using dictionary from (2)

Decide whether to do with:
    * filter out invalid lemmas or lemma+pos combinations
    * filter out invalid adjectives
-}

conll2Lemmas :: Corpus -> LemmaList
conll2Lemmas = 
  S.fromList . 
  concatMap (concatMap (map (T.pack . showLP) . lemmaPos) . sentenceTokens)

inList :: LemmaList -> LemmaPos -> Bool
inList ls (l,p) = T.pack (l ++ posSep ++ p) `S.member` ls

-- PTKVZ - abgetrennter Verbzusatz
ptkvz = "PTKVZ"

prefixPTKVZ :: LemmaList -> Sentence -> Sentence
prefixPTKVZ ls s = fmap prefix s
  where 
    ps = M.fromList . map (\t -> (dephead t, t)) . 
         filter ((==ptkvz) . postag) $ sentenceTokens s
    prefix t | postag t == "V" = 
                 case M.lookup (ix t) ps of
                   Just p  -> t { lemma = [ l' | p <- lemma p, l <- lemma t, 
                                  let l' = p++l, inList ls (l',cpostag t) ] }
                   Nothing -> t
             | otherwise = t

dehyphenate :: LemmaList -> Token -> Token
dehyphenate ls t = t { lemma = [ deh l | l <- lemma t ] }
  where deh l = let l' = removeHyphen l
                in if l'/=l && inList ls (l',cpostag t) then l' else l

removeHyphen :: String -> String
removeHyphen []     = []
removeHyphen (x:xs) = x : filter (/='-') (map toLower xs)

readLemmaList :: FilePath -> IO LemmaList
readLemmaList f = 
  S.fromList . map (head . T.words) . T.lines <$> T.readFile f

readLemmaDict :: FilePath -> IO LemmaDict
readLemmaDict f = M.fromList . map parse . T.lines <$> T.readFile f
  where parse s = case T.words s of
          (w:l:_) -> (w,l)
          _       -> error "no parse"

lemmatize :: LemmaDict -> Sentence -> Sentence
lemmatize d = fmap lemmatize
  where lemmatize t | unknownLemma t = backoff t
                    | otherwise      = t
        backoff t = let wp = T.pack $ form t ++ posSep ++ cpostag t
                    in case M.lookup wp d of
                        Just l' -> let (l,p) = lemmaPos l' in 
                                   t {lemma = [l], cpostag = p}
                        Nothing -> t
        lemmaPos l = case break (=='_') $ T.unpack l of
                       (l,_:p) -> (l,p)

main = undefined
