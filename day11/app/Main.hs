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
  | PanicS
  deriving (Show, Eq)

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

listWithIndices :: [(Int, [(Int, a)])] -> [((Int, Int), a)]
listWithIndices [] = []
listWithIndices ((rowIdx, []):rows) = listWithIndices rows
listWithIndices ((rowIdx, (colIdx, occ):cols):rows) =
  ((rowIdx, colIdx), occ) : listWithIndices ((rowIdx, cols) : rows)

environment :: (Int, Int) -> [(Int, Int)]
environment (x, y) =
  [ (x - 1, y - 1)
  , (x, y - 1)
  , (x + 1, y - 1)
  , (x - 1, y)
  , (x + 1, y)
  , (x - 1, y + 1)
  , (x, y + 1)
  , (x + 1, y + 1)
  ]

countOcc :: Map (Int, Int) Seat -> [(Int, Int)] -> Int
countOcc map [] = 0
countOcc map (x:xs) =
  (if M.findWithDefault PanicS x map == OccupiedS
     then 1
     else 0) +
  countOcc map xs
  where
    isOcc =
      case M.findWithDefault PanicS x map of
        PanicS -> error "panic!!!"
        OccupiedS -> True
        _ -> False

occupiedNeighCount :: Map (Int, Int) Seat -> (Int, Int) -> Seat -> Int
occupiedNeighCount map coords seat =
  if seat == OccupiedS || seat == EmptyS
    then countOcc map $ environment coords
    else 0

main :: IO ()
main = do
  contents <- readStdin
  let rows = enumerate $ L.map (enumerate . charToSeat) $ L.words contents
  let mappable = listWithIndices rows
  let map = M.fromList mappable
  let counts = M.mapWithKey (occupiedNeighCount map) map
  print map
  print counts
