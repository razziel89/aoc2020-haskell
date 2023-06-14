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

toInt :: String -> Int
toInt str = read str :: Int

outletJolts = 0

joltDiff = 3

numDiffs :: Int -> [Int] -> Int
numDiffs wantedDiff l = L.length $ L.filter (== wantedDiff) diffs
  where
    pairs = L.zip l $ L.drop 1 l
    diff (left, right) = abs (left - right)
    diffs = L.map diff pairs

zipWithConst :: a -> [a] -> [(a, a)]
zipWithConst val = L.map (, val)

addWhileGreater :: Int -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
addWhileGreater diff tup [] = []
addWhileGreater diff tup@(val, count) l@((jolt, joltCount):xs) =
  if val + diff >= jolt
    then (jolt, joltCount + count) : addWhileGreater diff tup xs
    else l

countConnections :: [(Int, Int)] -> [(Int, Int)]
countConnections [x] = [x]
countConnections (tup@(jolt, count):xs) =
  countConnections $ addWhileGreater joltDiff tup xs

main :: IO ()
main = do
  contents <- readStdin
  let jolts = L.map toInt $ L.words contents
  let deviceJolts = joltDiff + L.maximum jolts
  let sortedJolts = Sort.sort $ [0] ++ jolts ++ [deviceJolts]
  let part1 = numDiffs 1 sortedJolts * numDiffs 3 sortedJolts
  print part1
  -- Part 2
  let counts =
        [(outletJolts, 1)] ++
        zipWithConst 0 (Sort.sort jolts) ++ [(deviceJolts, 0)]
  let part2 = snd $ L.head $ countConnections counts
  print part2
