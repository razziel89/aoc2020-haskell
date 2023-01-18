{-# LANGUAGE TupleSections #-}

module Main where

import Data.Hashable
import Data.List as L
import Data.Map as M
import Data.Set as S
import Debug.Trace
import GHC.Generics (Generic)
import System.IO

readStdin :: IO String
readStdin = do
  readFile "/dev/stdin"

toInt :: String -> Int
toInt str = read str :: Int

noToZero :: String -> String
noToZero "no" = "0"
noToZero s = s

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn pred input =
  case dropWhile pred input of
    [] -> []
    l -> word : splitOn pred rest
      where (word, rest) = break pred l

count :: (Eq a) => a -> [a] -> Int
count val l = L.length $ L.filter (== val) l

elemCounts :: (Ord a) => [[a]] -> Map a Int
elemCounts ll = M.fromListWith (+) $ L.map (, 1) $ concat ll

applyToWords :: ([String] -> [String]) -> String -> String
applyToWords f s = L.unwords $ f $ L.words s

newtype Bag =
  Bag String
  deriving (Show, Ord, Eq)

data Def =
  Def Bag (Map Bag Int)
  deriving (Show)

parse :: String -> Def
parse l = Def bag map
  where
    line = L.filter (/= '.') l
    bag = Bag $ applyToWords (L.take 2) line
    secondHalf =
      L.map (applyToWords $ L.take 3) $
      splitOn (== ',') $ applyToWords (L.drop 4) line
    map =
      M.fromList $
      L.filter ((/= 0) . snd) $
      L.map
        (\s ->
           ( Bag $ applyToWords (L.drop 1) s
           , toInt $ noToZero $ applyToWords (L.take 1) s))
        secondHalf

main :: IO ()
main = do
  contents <- readStdin
  -- Part 1.
  let groups = L.map parse $ lines contents
  print groups
