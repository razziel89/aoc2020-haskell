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

splitOn :: (Char -> Bool) -> String -> [String]
splitOn pred inputStr =
  case dropWhile pred inputStr of
    "" -> []
    str -> word : splitOn pred rest
      where (word, rest) = break pred str

flipTup :: (a, b) -> (b, a)
flipTup (x, y) = (y, x)

nextWithDiff :: Int -> Int -> Int -> Int -> Int
nextWithDiff step diff curr mult =
  if curr `mod` mult == diff
    then curr
    else nextWithDiff step diff (curr + step) mult

solvePart2 :: [(Int, Int)] -> Int -> Int -> Int
solvePart2 [] curr step = curr
solvePart2 ((diff, mult):xs) curr step =
  solvePart2 xs (nextWithDiff step diff curr mult) (mult * step)

sndSubFstWithMod :: (Int, Int) -> (Int, Int)
sndSubFstWithMod (x, y) = ((y - x) `mod` y, y)

main :: IO ()
main = do
  contents <- readStdin
  let eta = toInt $ L.head $ L.words contents
  let parsed =
        L.map toInt $
        L.filter (/= "x") $ splitOn (== ',') $ L.words contents !! 1
  let possible =
        L.map (flipTup . second (\s -> eta + s - (eta `mod` s))) $
        enumerate parsed
  let earliest = L.minimum possible
  let busID = parsed !! snd earliest
  let part1 = (fst earliest - eta) * busID
  print part1
  let parsed2 =
        L.map (sndSubFstWithMod . second toInt) $
        L.filter (\s -> snd s /= "x") $
        enumerate $ splitOn (== ',') $ L.words contents !! 1
  let part2 = solvePart2 parsed2 1 1
  print part2
