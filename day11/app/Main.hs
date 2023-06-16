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

enumerate :: [a] -> [(Int, a)]
enumerate = L.zip [0 ..]

data Seat
  = EmptyS
  | OccupiedS
  | FloorS
  deriving (Show)

charToSeat :: [Char] -> [Seat]
charToSeat [] = []
charToSeat (x:xs) = converted : charToSeat xs
  where
    converted =
      case x of
        'L' -> EmptyS
        '.' -> FloorS
        '#' -> OccupiedS
        _ -> error "cannot happen"

listWithIndices :: [(Int, [(Int, a)])] -> [(Int, Int, a)]
listWithIndices [] = []
listWithIndices ((rowIdx, []):rows) = listWithIndices rows
listWithIndices ((rowIdx, (colIdx, occ):cols):rows) =
  (rowIdx, colIdx, occ) : listWithIndices ((rowIdx, cols) : rows)

main :: IO ()
main = do
  contents <- readStdin
  let rows = enumerate $ L.map (enumerate . charToSeat) $ L.words contents
  let mappable = listWithIndices rows
  print mappable
