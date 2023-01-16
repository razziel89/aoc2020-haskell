{-# LANGUAGE
    DeriveGeneric
 #-}
 module Main where

import System.IO
import System.IO (isEOF)
import Data.List as L
import Data.Map as M
import Data.Set as S
import GHC.Generics (Generic)
import Data.Hashable

readStdin :: IO String
readStdin = do
  contents <- readFile "/dev/stdin"
  return contents

-- enumerate :: [a] -> [(Int, a)]
-- enumerate l = zip [0..] l

-- toInt :: String -> Int
-- toInt str = read str :: Int

-- inRange :: (Ord a) => a -> a -> a -> Bool
-- inRange min max val = min <= val && val <= max

-- canBeInt :: String -> Bool
-- canBeInt l = L.all (inRange '0' '9')  l

-- splitOn :: (a -> Bool) -> [a] -> [[a]]
-- splitOn pred input =
--   case dropWhile pred input of
--     [] -> []
--     l -> word : (splitOn pred rest)
--       where (word, rest) = break pred l

hasLength :: Int -> String -> Bool
hasLength len val = (length val) == len

-- reverseList :: [a] -> [a]
-- reverseList xs = L.foldl (\x y -> y:x) [] xs

-- endsWith :: (Eq a) => [a] -> [a] -> Bool
-- endsWith ext l = L.all (\entry -> (fst entry) == (snd entry)) $ zip revExt revL
--   where
--     revExt = reverseList ext
--     revL = reverseList l

-- hasElem :: (Eq a) => a -> [a] -> Bool
-- hasElem e [] = False
-- hasElem e (x:xs) = if x == e then True else hasElem e xs

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

unNest :: ((Int, Int), (Int, Int)) -> (Int, Int)
unNest ((i, j), (x, y)) = (i, x)

toSeatId :: (Int, Int) -> Int
toSeatId (row, col) = row * 8 + col

maxElem :: (Ord a) => [a] -> a
maxElem (x:[]) = x
maxElem (x:xs) = if x > restMax then x else restMax
  where
    restMax = maxElem xs

numSeats = 128 * 8 :: Int

numToSpread :: Int -> (Int, Int, Int)
numToSpread i = (i-1, i, i+1)

isCandidateSpread :: (Ord k) => Set k -> (k, k, k) -> Bool
isCandidateSpread s (x, y, z) = S.member x s && (not $ S.member y s) && S.member z s

mid :: (a, a, a) -> a
mid (a, b, c) = b

main :: IO ()
main = do
  contents <- readStdin
  -- Part 1.
  let parsed = L.map parse $ lines contents
  let split = L.map (L.splitAt 7) parsed
  let partitioned = L.map (\x -> ((binPart (0, 127) $ fst x), (binPart (0, 7) $ snd x))) split
  let seatIds = L.map toSeatId $ L.map unNest partitioned
  -- putStrLn (show seatIds)
  let largestId = maxElem seatIds
  putStrLn (show largestId)
  -- Part 2
  let seatIdsSet = S.fromList seatIds
  -- We cannot be sitting at the very front or back.
  let candidates = head $ L.map mid $ L.filter (isCandidateSpread seatIdsSet) $ L.map numToSpread [1..numSeats-2]
  putStrLn (show candidates)
