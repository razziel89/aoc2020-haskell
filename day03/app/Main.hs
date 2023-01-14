{-# LANGUAGE
    DeriveGeneric
 #-}
 module Main where

import System.IO
import System.IO (isEOF)
import Data.List as L
import Data.Map as M
import GHC.Generics (Generic)
import Data.Hashable

readStdin :: IO String
readStdin = do
  contents <- readFile "/dev/stdin"
  return contents

data Vector = Vector Int Int
  deriving (Generic, Show, Ord, Eq)
instance Hashable Vector where
  hash (Vector x y) = ((hash x) * 27139 + (hash y))
vecX (Vector x _) = x
vecY (Vector _ y) = y
fromTuple (x, y) = Vector x y

toIndexed :: (Int, [(Int, Char)]) -> [(Vector, Bool)]
toIndexed input = L.map (\(ix, val) -> (Vector ix iy, val == '.')) innerList
  where (iy, innerList) = input

fileToMap :: String -> Map Vector Bool
fileToMap s = M.fromList (concat $ L.map toIndexed $ enumerated)
  where
    asLines = lines s
    enumerated = enumerate $ L.map enumerate asLines

enumerate :: [a] -> [(Int, a)]
enumerate l = zip [0..] l

toInt :: String -> Int
toInt str = read str :: Int

splitOn :: (Char -> Bool) -> String -> [String]
splitOn pred inputStr =
  case dropWhile pred inputStr of
    "" -> []
    str -> word : (splitOn pred rest)
      where (word, rest) = break pred str

add :: Vector -> Vector -> Vector
add v1 v2 = Vector ((vecX v1) + (vecX v2)) ((vecY v1) + (vecY v2))

part1Direction = Vector 3 1

path :: Int -> Vector -> [Vector]
path maxY dir = L.map fromTuple $ zip xRange yRange
  where
    yRange = [(vecY dir), (2*(vecY dir)) .. maxY]
    xRange = [(vecX dir), (2*(vecX dir)) ..]

mapElem :: (Hashable k, Ord k) => Map k a -> k -> Maybe a 
mapElem m k = M.lookup k m

-- path :: (((Int, Int), Char, String) -> Bool) -> [String] -> Int
-- path numRows m = L.map lookup $ path numRows part1Direction
  -- where
  --   lookup k = M.lookup k m

main :: IO ()
main = do
  file <- readStdin
  let numRows = length $ lines file
  let numCols = length $ head $ lines file
  let chars = fileToMap file
  let part1Path = path numRows part1Direction
  putStrLn (show part1Path)
  let vals = L.map (mapElem chars) part1Path
  putStrLn (show vals)
  -- let part2 = path solutionPart2 lines
  -- putStrLn (show part2)
