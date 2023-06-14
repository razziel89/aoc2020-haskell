{-# LANGUAGE TupleSections #-}

module Main where

import Data.Bifunctor as BF
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

toInt :: String -> Int
toInt str = read str :: Int

inRange :: (Ord a) => a -> a -> a -> Bool
inRange min max val = min <= val && val <= max

myTrace :: (Show a) => String -> a -> a
myTrace msg a = traceShow (msg ++ " " ++ show a) a

outletJolts :: Int
outletJolts = 0

joltDiff :: Int
joltDiff = 3

numDiffs :: Int -> [Int] -> Int
numDiffs wantedDiff l = L.length $ L.filter (== wantedDiff) diffs
  where
    pairs = L.zip l $ L.drop 1 l
    diff (left, right) = abs (left - right)
    diffs = L.map diff pairs

main :: IO ()
main = do
  contents <- readStdin
  let jolts = L.map toInt $ L.words contents
  let deviceJolts = L.maximum jolts + joltDiff
  let part1 = Sort.sort $ [0] ++ jolts ++ [deviceJolts]
  let solution = numDiffs 1 part1 * numDiffs 3 part1
  print (part1, solution, L.length part1, L.length jolts)
