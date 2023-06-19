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

toInt :: String -> Int
toInt str = read str :: Int

data Dir
  = NorthD
  | SouthD
  | EastD
  | WestD
  deriving (Show, Eq)

data State =
  State Dir (Int, Int)
  deriving (Show)

start = State EastD (0, 0)

data Action
  = NorthA
  | SouthA
  | EastA
  | WestA
  | LeftA
  | RightA
  | ForwardA
  deriving (Show, Eq)

actionFromStr :: Char -> Action
actionFromStr s =
  case s of
    'N' -> NorthA
    'S' -> SouthA
    'E' -> EastA
    'W' -> WestA
    'L' -> LeftA
    'R' -> RightA
    'F' -> ForwardA

actionFromDir :: Dir -> Action
actionFromDir dir =
  case dir of
    NorthD -> NorthA
    SouthD -> SouthA
    EastD -> EastA
    WestD -> WestA

dirFromAction :: (Action, Int) -> Dir -> Dir
dirFromAction (_, 0) dir = dir
dirFromAction (act, dist) dir =
  case act of
    LeftA ->
      case dir of
        NorthD -> dirFromAction (act, dist - 90) WestD
        SouthD -> dirFromAction (act, dist - 90) EastD
        EastD -> dirFromAction (act, dist - 90) NorthD
        WestD -> dirFromAction (act, dist - 90) SouthD
    RightA ->
      case dir of
        NorthD -> dirFromAction (act, dist - 90) EastD
        SouthD -> dirFromAction (act, dist - 90) WestD
        EastD -> dirFromAction (act, dist - 90) SouthD
        WestD -> dirFromAction (act, dist - 90) NorthD
    _ -> dir

addVec :: (Int, Int) -> (Int, Int) -> (Int, Int)
addVec (x, y) (a, b) = (x + a, y + b)

parse :: String -> (Action, Int)
parse s = (actionFromStr $ L.head s, toInt $ L.drop 1 s)

translate :: State -> (Action, Int) -> State
translate curr@(State dir coords) (act, dist) =
  case act of
    NorthA -> State newdir (addVec coords (0, -dist))
    SouthA -> State newdir (addVec coords (0, dist))
    EastA -> State newdir (addVec coords (-dist, 0))
    WestA -> State newdir (addVec coords (dist, 0))
    LeftA -> State newdir coords
    RightA -> State newdir coords
    ForwardA -> translate curr (myTrace "act" $ actionFromDir dir, dist)
  where
    newdir = dirFromAction (act, dist) dir

main :: IO ()
main = do
  contents <- readStdin
  let parsed = L.map parse $ L.words contents
  let final = L.foldl translate start parsed
  let State _ (x, y) = final
  print parsed
  print (abs x + abs y)
