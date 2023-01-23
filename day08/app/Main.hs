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

apply :: ((String, Int), Int, Int, Bool) -> (Int, Int, Bool)
apply ((op, val), acc, idx, swap) =
  case op of
    "nop" ->
      if swap
        then (acc, idx + val, False)
        else (acc, idx + 1, False)
    "acc" -> (acc + val, idx + 1, False)
    "jmp" ->
      if swap
        then (acc, idx + 1, False)
        else (acc, idx + val, False)
    "stp" -> (acc, idx, True)

sec (a, b, c) = b

thi (a, b, c) = c

multiApply ::
     [(String, Int)] -> Int -> (Int, Int, Bool) -> Set Int -> (Int, Int, Bool)
multiApply ops swapIdx (acc, next, stopped) set = newVal
  where
    alreadyVisited = S.member next set
    largerSet = S.union set (S.fromList [next])
    op = ops !! next
    newNext = apply (op, acc, next, next == swapIdx)
    newVal =
      if alreadyVisited
        then (next, acc, thi newNext)
        else multiApply ops swapIdx newNext largerSet

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

main :: IO ()
main = do
  contents <- readStdin
  -- Somehow, "+4" cannot be parsed as a positive integer... Thus, we remove
  -- that character from the input before parsing it.
  let parsed =
        L.map (BF.second (toInt . L.filter (/= '+')) . listToTuple . L.words) $
        lines contents
  let visited = S.empty
  -- We use "-1" as swap index because we don't actually want to swap any
  -- operation.
  let applied = sec $ multiApply parsed (-1) (0, 0, False) S.empty
  print applied
  -- Part 2.
  let parsed2 = parsed ++ [("stp", 0)]
  let swapIndices =
        L.map fst $
        L.filter (\e -> (fst . snd) e == "jmp" || (fst . snd) e == "nop") $
        enumerate parsed2
  let part2 =
        L.filter
          (\e -> thi $ multiApply parsed2 e (0, 0, False) S.empty)
          swapIndices
  let applied2 = sec $ multiApply parsed2 (L.head part2) (0, 0, False) S.empty
  print applied2
