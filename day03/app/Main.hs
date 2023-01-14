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
add (Vector x1 y1) (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)

path :: Int -> Vector -> [Vector]
path maxY (Vector dirX dirY)= L.map fromTuple $ zip xRange yRange
  where
    yRange = [(dirY), (2*(dirY)) .. maxY-1]
    xRange = [(dirX), (2*(dirX)) ..]

mapElem :: (Hashable k, Ord k) => Map k a -> k -> Maybe a 
mapElem m k = M.lookup k m

modX :: Int -> Vector -> Vector
modX m (Vector x y) = Vector (mod x m) y

countJust :: (Eq a) => a -> [Maybe a] -> Int
countJust _ [] = 0
countJust cmp ((Just actual):xs) = count + rest
  where
    rest = countJust cmp xs
    count = if actual == cmp then 1 else 0

countTrees :: String -> Vector -> Int
countTrees contents dir = countJust False $ L.map (mapElem chars) $ L.map (modX numCols) thisPath
  where 
    numRows = length $ lines contents
    numCols = length $ head $ lines contents
    thisPath = path numRows dir
    chars = fileToMap contents

part1Direction = Vector 3 1

part2Directions =
  [ Vector 1 1
  , Vector 3 1
  , Vector 5 1
  , Vector 7 1
  , Vector 1 2 ]

main :: IO ()
main = do
  contents <- readStdin
  let valsPart1 = countTrees contents part1Direction
  putStrLn (show valsPart1)
  let valsPart2 = L.map (countTrees contents) part2Directions
  putStrLn (show $ L.product valsPart2)
