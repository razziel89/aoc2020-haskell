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

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn pred input =
  case dropWhile pred input of
    [] -> []
    l -> word : (splitOn pred rest)
      where (word, rest) = break pred l

count :: (Eq a) => a -> [a] -> Int
count val l = L.length $ L.filter (==val) l

elemCounts :: (Ord a) => [[a]] -> Map a Int
elemCounts ll = M.fromListWith (+) $ L.map (\v -> (v, 1)) $ concat ll

main :: IO ()
main = do
  contents <- readStdin
  -- Part 1.
  let groups = splitOn (=="") $ lines contents
  let part1 = L.foldl (+) 0 $ L.map (\g -> S.size $ S.fromList $ concat g) groups
  putStrLn (show part1)
  -- Part 2.
  -- Keep only those values that have been specified as often as the list is
  -- long because the list has a length of the number of people in the list.
  let listCounts = L.map (\g -> M.size $ M.filter (==(L.length g)) $ elemCounts g) groups
  let part2 = L.foldl (+) 0 listCounts
  putStrLn (show part2)
