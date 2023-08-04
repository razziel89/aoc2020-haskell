{-# LANGUAGE TupleSections #-}

module Main where

import Data.Bifunctor as BF
import Data.Bits as B
import Data.Hashable
import Data.List as L
import Data.Map as M
import Data.Maybe as May
import Data.Sequence as Seq
import Data.Set as S
import Data.Sort as Sort
import Debug.Trace
import GHC.Generics (Generic)
import System.IO

readStdin :: IO String
readStdin = do
  readFile "/dev/stdin"

enumerate :: [a] -> [(Int, a)]
enumerate = L.zip [0 ..]

myTrace :: (Show a) => String -> a -> a
myTrace msg a = traceShow (msg ++ " " ++ show a) a

toInt :: String -> Int
toInt str = read str :: Int

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn pred input =
  case dropWhile pred input of
    [] -> []
    str -> word : splitOn pred rest
      where (word, rest) = break pred str

pairUp :: [a] -> [(a, a)]
pairUp (x:y:xs) = (x, y) : pairUp xs
pairUp [] = []
pairUp l = error "cannot pair up values"

parseEntry :: [String] -> (String, [(Int, Int)])
parseEntry (x:xs) = (x, pairUp $ L.map toInt xs)

parse :: [String] -> [(String, [(Int, Int)])]
parse words =
  L.map parseEntry $
  splitOn (== "mask") $
  L.filter (\s -> s /= "mem" && s /= "=") $
  L.concatMap (splitOn (\s -> s == '[' || s == ']')) words

main :: IO ()
main = do
  contents <- readStdin
  let parsed = parse $ L.words contents
  print parsed
