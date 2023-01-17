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
count val (x:xs) = (if val == x then 1 else 0) + count val xs

-- listCounts :: [a] -> Map a Int
--
-- filterValues :: v -> Map k v -> Map k v
-- filterValues val map = M.filter (==val)

main :: IO ()
main = do
  contents <- readStdin
  -- Part 1.
  let groups = splitOn (=="") $ lines contents
  let part1 = L.foldl (+) 0 $ L.map S.size $ L.map S.fromList $ L.map concat groups
  putStrLn (show part1)
  -- -- Part 2.
  -- -- Keep only those values that have been specified as often as the list
  -- let realListCount = L.map (\g -> filterValues (L.length g) $ listCounts g) groups
  -- let groupSets = L.fold (+) $ L.map (\s -> S.filter (\s -> S.size s) == )
