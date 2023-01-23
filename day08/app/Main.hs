{-# LANGUAGE TupleSections #-}

module Main where

import Data.Bifunctor as BF
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

listToTuple :: [a] -> (a, a)
listToTuple l = (head, second)
  where
    head = L.head l
    second =
      if L.length l == 2
        then l !! 1
        else error "list has length /= 2"

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn pred input =
  case dropWhile pred input of
    [] -> []
    l -> word : splitOn pred rest
      where (word, rest) = break pred l

lookupWithPanic :: (Ord k) => Map k [v] -> k -> [v]
lookupWithPanic m k =
  if M.member k m
    then M.findWithDefault [] k m
    else error "cannot find entry"

myTrace :: (Show a) => String -> a -> a
myTrace msg a = traceShow (msg ++ " " ++ show a) a

apply :: ((String, Int), Int, Int) -> (Int, Int)
apply ((op, val), acc, idx) =
  case op of
    "nop" -> (acc, idx + 1)
    "acc" -> (acc + val, idx + 1)
    "jmp" -> (acc, idx + val)

multiApply :: [(String, Int)] -> (Int, Int) -> Set Int -> (Int, Int)
multiApply ops (acc, next) set = newVal
  where
    alreadyVisited = S.member next set
    largerSet = S.union set (S.fromList [next])
    op = ops !! next
    newNext = apply (op, acc, next)
    newVal =
      if alreadyVisited
        then (next, acc)
        else multiApply ops newNext largerSet

main :: IO ()
main = do
  contents <- readStdin
  -- Somehow, "+4" cannot be parsed as a positive integer... Thus, we remove
  -- that character from the input before parsing it.
  let parsed =
        L.map (BF.second (toInt . L.filter (/= '+')) . listToTuple . L.words) $
        lines contents
  let visited = S.empty
  -- print parsed
  let applied = snd $ multiApply parsed (0, 0) S.empty
  print applied
