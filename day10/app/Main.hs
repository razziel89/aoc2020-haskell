{-# LANGUAGE TupleSections #-}

module Main where

import Data.Bifunctor as BF
import Data.Hashable
import Data.List as L
import Data.Map as M
import Data.Maybe as May
import Data.Sequence as Seq
import Data.Set as S
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

possibleAdapters :: Int -> [Int] -> ([Int], [Int])
possibleAdapters jolt l = (possible, rest)
  where
    range = inRange jolt (jolt + joltDiff)
    possible = L.filter range l
    rest = L.filter (not . range) l

applyToTuple :: (a -> b -> c) -> (a, b) -> c
applyToTuple f (left, right) = f left right

rmFirst :: Eq a => a -> [a] -> [a]
rmFirst val [] = []
rmFirst val (x:xs) =
  if val == x
    then xs
    else x : rmFirst val xs

solvePart1 :: Int -> [Int] -> [Int] -> [Int] -> (Bool, [Int])
-- End reached.
solvePart1 end l [] [] = (L.last l == end, l)
-- No new adapter to choose from.
solvePart1 end l [] remainder = (False, [])
solvePart1 end trackSoFar nextJolts remainder = (result, track)
  where
    possibleSolution jolt =
      applyToTuple (solvePart1 end (myTrace "track" $ jolt : trackSoFar)) $
      possibleAdapters jolt $
      remainder ++ rmFirst (myTrace "jolt" jolt) nextJolts
    filtered = L.filter fst $ L.map possibleSolution nextJolts
    (result, track) =
      if L.null filtered
        then (False, [])
        else (True, snd $ L.head filtered)

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
  let (possibleStart, remainder) = possibleAdapters outletJolts jolts
  let (_, middle) = solvePart1 deviceJolts [0] possibleStart remainder
  let part1 = L.reverse $ deviceJolts : middle
  let solution = numDiffs 1 part1 * numDiffs 3 part1
  print (part1, solution, L.length part1, L.length jolts)
