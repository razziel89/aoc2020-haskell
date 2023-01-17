{-# LANGUAGE
  DeriveGeneric
#-}
module Main where

import System.IO
import Data.List as L
import Data.Map as M
import Data.Set as S
import GHC.Generics (Generic)
import Data.Hashable

readStdin :: IO String
readStdin = do
  contents <- readFile "/dev/stdin"
  return contents

-- Parse determines whether to keep the upper half.
parse :: String -> [Bool]
parse s = L.map (\x -> x == 'B' || x == 'R') s

binPart :: (Int, Int) -> [Bool] -> (Int, Int)
binPart tup [] = tup
binPart (low, high) (x:xs) = binPart (newLow, newHigh) xs
  where
    range = (high - low) + 1
    halfRange = div range 2 :: Int
    newLow = if x then low + halfRange else low
    newHigh = if x then high else high - halfRange

nestedBothSame :: ((Int, Int), (Int, Int)) -> Bool
nestedBothSame ((i, j), (x, y)) = i == j && x == y

-- Un-next those double tuple constructs and panic if they are not the same 
unNest :: ((Int, Int), (Int, Int)) -> (Int, Int)
unNest t = (i, x)
  where ((i, j), (x, y)) = if nestedBothSame t then t else error "not the same"

toSeatId :: (Int, Int) -> Int
toSeatId (row, col) = row * 8 + col

maxElem :: (Ord a) => [a] -> a
maxElem (x:[]) = x
maxElem (x:xs) = if x > restMax then x else restMax
  where restMax = maxElem xs

numSeats = 128 * 8 :: Int

numToSpread :: Int -> (Int, Int, Int)
numToSpread i = (i-1, i, i+1)

-- Our neighbouring seats need to be part of the set but the candidate seat
-- isn't.
isCandidateSpread :: (Ord k) => Set k -> (k, k, k) -> Bool
isCandidateSpread s (x, y, z) = S.member x s && (not $ S.member y s) && S.member z s

mid :: (a, a, a) -> a
mid (a, b, c) = b

seatIds :: [String] -> [Int]
seatIds lines = L.map toSeatId $ L.map unNest partitioned
  where
    parsed = L.map parse lines
    split = L.map (L.splitAt 7) parsed
    partitioned = L.map (\x -> ((binPart (0, 127) $ fst x), (binPart (0, 7) $ snd x))) split

main :: IO ()
main = do
  contents <- readStdin
  -- Part 1.
  let realSeatIds = seatIds $ lines contents
  let largestId = maxElem realSeatIds
  putStrLn (show largestId)
  -- Part 2
  let seatIdsSet = S.fromList realSeatIds
  -- We cannot be sitting at the very front or back, which is why the interval
  -- will not be the full [0..numSeats-1].
  let candidates = head $ L.map mid $ L.filter (isCandidateSpread seatIdsSet) $ L.map numToSpread [1..numSeats-2]
  putStrLn (show candidates)
