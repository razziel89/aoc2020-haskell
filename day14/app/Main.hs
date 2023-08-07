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

enumerate :: [a] -> [(Integer, a)]
enumerate = L.zip [0 ..]

myTrace :: (Show a) => String -> a -> a
myTrace msg a = traceShow (msg ++ " " ++ show a) a

toInt :: String -> Integer
toInt str = read str :: Integer

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn pred input =
  case dropWhile pred input of
    [] -> []
    str -> word : splitOn pred rest
      where (word, rest) = break pred str

orMask :: String -> Integer -> Integer
orMask [] val = val
orMask (x:xs) val = orMask xs nextVal
  where
    nextVal = B.shiftL val 1 .|. thisBit
    thisBit =
      case x of
        'X' -> B.zeroBits
        '0' -> B.zeroBits
        '1' -> B.bit 0

startOrMask :: Integer
startOrMask = B.zeroBits

andMask :: String -> Integer -> Integer
andMask [] val = val
andMask (x:xs) val = andMask xs nextVal
  where
    nextVal = B.shiftL val 1 .|. thisBit
    thisBit =
      case x of
        'X' -> B.bit 0
        '1' -> B.bit 0
        '0' -> B.clearBit val 0

numBits :: Integer
numBits = 36

dupChar :: Char -> Integer -> String
dupChar c 0 = []
dupChar c i = c : dupChar c (i - 1)

intToBin :: Integer -> String
intToBin 0 = "0"
intToBin 1 = "1"
intToBin i = intToBin (i `div` 2) ++ intToBin (i `mod` 2)

startAndMask :: Integer
startAndMask = orMask (dupChar '1' numBits) startOrMask

applyMask :: String -> Integer -> Integer
applyMask mask org = org

pairUp :: [a] -> [(a, a)]
pairUp (x:y:xs) = (x, y) : pairUp xs
pairUp [] = []
pairUp l = error "cannot pair up values"

parseEntry :: [String] -> ((Integer, Integer), [(Integer, Integer)])
parseEntry (x:xs) = ((maskAnd, maskOr), pairUp $ L.map toInt xs)
  where
    maskAnd = andMask x startAndMask
    maskOr = orMask x startOrMask

parse :: [String] -> [((Integer, Integer), [(Integer, Integer)])]
parse words =
  L.map parseEntry $
  splitOn (== "mask") $
  L.filter (\s -> s /= "mem" && s /= "=") $
  L.concatMap (splitOn (\s -> s == '[' || s == ']')) words

main :: IO ()
main = do
  contents <- readStdin
  let parsed = parse $ L.words contents
  print parsed
  print $ intToBin startOrMask
  print $ intToBin startAndMask
  let ((and, or), _) = L.head parsed
  print $ intToBin $ or .|. (and .&. 0)
