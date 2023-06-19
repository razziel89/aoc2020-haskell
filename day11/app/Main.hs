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

myTrace :: (Show a) => String -> a -> a
myTrace msg a = traceShow (msg ++ " " ++ show a) a

data Seat
  = EmptyS
  | OccupiedS
  | FloorS
  -- This value will result in a panic, it is there because working with maybes
  -- and error handling is a pain.
  | PanicS
  deriving (Show, Eq)

printSeat :: Seat -> Char
printSeat seat =
  case seat of
    EmptyS -> 'L'
    FloorS -> '.'
    OccupiedS -> '#'
    _ -> error "cannot happen"

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
  ((colIdx, rowIdx), occ) : listWithIndices ((rowIdx, cols) : rows)

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

occupiedNeighCount :: Map (Int, Int) Seat -> (Int, Int) -> Seat -> (Seat, Int)
occupiedNeighCount map coords seat =
  if seat == OccupiedS || seat == EmptyS
    then (seat, countOcc map $ environment coords)
    else (seat, 0)

occupiedVisibleCount :: Map (Int, Int) Seat -> (Int, Int) -> Seat -> (Seat, Int)
occupiedVisibleCount map coords seat =
  if seat == OccupiedS || seat == EmptyS
    then (seat, countOcc map $ environment coords)
    else (seat, 0)

nextState :: Int -> (Seat, Int) -> Seat
nextState maxNeigh (seat, neighCount) =
  case seat of
    EmptyS ->
      if neighCount == 0
        then OccupiedS
        else seat
    OccupiedS ->
      if neighCount >= maxNeigh
        then EmptyS
        else seat
    PanicS -> error "cannot happen"
    _ -> seat

nextMapState :: Int -> Map (Int, Int) (Seat, Int) -> Map (Int, Int) Seat
nextMapState maxNeigh = M.map (nextState maxNeigh)

printMapWithSize :: (Int, Int) -> (Int, Int) -> Map (Int, Int) Seat -> String
printMapWithSize coord@(x, y) max@(xmax, ymax) map = result
  where
    seat = M.findWithDefault PanicS coord map
    result =
      if x == xmax
        then if y == ymax
               then printSeat seat : "\n\n"
               else printSeat seat : '\n' : printMapWithSize (0, y + 1) max map
        else printSeat seat : printMapWithSize (x + 1, y) max map

intSqrt :: Int -> Int
intSqrt = floor . sqrt . fromIntegral

addToFst :: Int -> (Int, a) -> (Int, a)
addToFst add (val, any) = (add + val, any)

iterateUntilUnchanged ::
     Int
  -> (Map (Int, Int) Seat -> String)
  -> (Map (Int, Int) Seat -> (Int, Int) -> Seat -> (Seat, Int))
  -> Map (Int, Int) Seat
  -> (Int, Map (Int, Int) Seat)
iterateUntilUnchanged maxNeigh printFn occNeighCountFn map = result
  where
    counts = M.mapWithKey (occNeighCountFn map) map
    nextMap = nextMapState maxNeigh counts
    changed = myTrace (printFn nextMap) $ map /= nextMap
    result =
      if changed
        then addToFst 1 $
             iterateUntilUnchanged maxNeigh printFn occNeighCountFn nextMap
        else (0, map)

countTotalOcc :: Map (Int, Int) Seat -> Int
countTotalOcc map =
  L.length $ L.filter (== OccupiedS) $ L.map snd $ M.toList map

main :: IO ()
main = do
  contents <- readStdin
  let rows = enumerate $ L.map (enumerate . charToSeat) $ L.words contents
  let mappable = listWithIndices rows
  let map = M.fromList mappable
  let sqrt = intSqrt (L.length mappable) - 1
  let printFn = printMapWithSize (0, 0) (sqrt, sqrt)
  let (_, endMap) = iterateUntilUnchanged 4 printFn occupiedNeighCount map
  putStrLn $ printFn endMap
  print $ countTotalOcc endMap
  let (_, endMap2) = iterateUntilUnchanged 5 printFn occupiedVisibleCount map
  putStrLn $ printFn endMap2
  print $ countTotalOcc endMap2
