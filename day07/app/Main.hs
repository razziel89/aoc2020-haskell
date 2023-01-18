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

toIntWithNo :: String -> Int
toIntWithNo "no" = 0
toIntWithNo str = read str :: Int

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
    words = L.words $ L.filter (/= '.') l
    bag = Bag $ L.unwords $ L.take 2 words
    secondHalf =
      L.map (applyToWords $ L.take 3) $
      splitOn (== ',') $ L.unwords $ L.drop 4 words
    map =
      M.fromList $
      L.filter (\t -> snd t /= 0) $
      L.map
        (\s ->
           ( Bag $ applyToWords (L.drop 1) s
           , toIntWithNo $ applyToWords (L.take 1) s))
        secondHalf

main :: IO ()
main = do
  contents <- readStdin
  -- Part 1.
  let groups = L.map parse $ lines contents
  print groups
