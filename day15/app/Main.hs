{-# LANGUAGE TupleSections #-}

module Main where

import Data.Bifunctor as BF
import Data.Bits as B
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

myConvTrace :: (Show b) => String -> (a -> b) -> a -> a
myConvTrace msg f a = traceShow (msg ++ " " ++ show (f a)) a

toInt :: String -> Int
toInt str = read str :: Int

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn pred input =
  case dropWhile pred input of
    [] -> []
    str -> word : splitOn pred rest
      where (word, rest) = break pred str

swap :: (Int, Int) -> (Int, Int)
swap (a, b) = (b, a)

evolve :: Int -> M.Map Int Int -> Int -> Int
evolve 2020 lastMap realLast = realLast
evolve count lastMap realLast = evolve (count + 1) nextMap next
  where
    lastSpoken = M.findWithDefault (-1) realLast lastMap
    next =
      if lastSpoken == -1
        then 0
        else count - 1 - lastSpoken
    nextMap = M.insert realLast (count - 1) lastMap

evolvePart2 :: Int -> M.Map Int Int -> Int -> Int
evolvePart2 30000000 lastMap realLast = realLast
evolvePart2 count lastMap realLast = evolve (count + 1) nextMap next
  where
    lastSpoken = M.findWithDefault (-1) realLast lastMap
    next =
      if lastSpoken == -1
        then 0
        else count - 1 - lastSpoken
    nextMap = M.insert realLast (count - 1) lastMap

main :: IO ()
main = do
  contents <- readStdin
  let parsed = L.map toInt $ splitOn (== ',') contents
  print parsed
  let last =
        M.fromList $
        L.map swap $ L.take (L.length parsed - 1) $ enumerate parsed
  print last
  let evolvedPart1 = evolve (L.length parsed) last (L.last parsed)
  print evolvedPart1
  let evolvedPart2 = evolvePart2 (L.length parsed) last (L.last parsed)
  print evolvedPart2
