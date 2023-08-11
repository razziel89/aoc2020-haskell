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
        'X' -> 0
        '0' -> 0
        '1' -> B.bit 0

startOrMask :: Integer
startOrMask = B.zeroBits

andMask :: String -> Integer -> Integer
andMask mask startVal = L.foldl (.&.) startVal $ L.map toBit withIdx
  where
    maxIdx = L.length mask - 1
    withIdx = L.map (applyToFst (maxIdx -)) $ enumerate mask
    toBit :: (Int, Char) -> Integer
    toBit (idx, char) =
      case char of
        'X' -> startVal
        '1' -> startVal
        '0' -> B.clearBit startVal idx

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

maskNum :: (Integer, Integer) -> Integer -> Integer
maskNum (and, or) val = or .|. (and .&. val)

applyToSnd :: (a -> b) -> (c, a) -> (c, b)
applyToSnd f (a, b) = (a, f b)

applyToFst :: (a -> b) -> (a, c) -> (b, c)
applyToFst f (a, b) = (f a, b)

mapFstOverSnds :: (a -> b, [(c, a)]) -> [(c, b)]
mapFstOverSnds (f, l) = L.map (applyToSnd f) l

masksToBin :: (Integer, Integer) -> (String, String)
masksToBin (i, j) = (intToBin i, intToBin j)

indicesWithOnes :: Integer -> [Int]
indicesWithOnes i =
  L.map fst $ L.filter ((== '1') . snd) $ enumerate $ intToBin i

masksPart2 :: (Integer, Integer) -> (Integer, [Int])
masksPart2 (and, or) = (or, indicesWithOnes (and `B.xor` or))

dupBitAtIdx :: Int -> [Integer] -> [Integer]
dupBitAtIdx idx = L.concatMap dup
  where
    dup val = [B.setBit val idx, B.clearBit val idx]

shortList :: a -> [a]
shortList a = [a]

dupBitAtIndices :: [Integer] -> [Int] -> [Integer]
dupBitAtIndices masks [] = []
dupBitAtIndices masks (x:xs) = applied ++ dupBitAtIndices applied xs
  where
    applied = dupBitAtIdx x masks

dupBits :: Integer -> [Int] -> [Integer]
dupBits mask l =
  myConvTrace "dupped" (L.map intToBin) $
  S.toList $ S.fromList $ dupBitAtIndices [mask] l

applyToTuple :: (a -> b -> c) -> (a, b) -> c
applyToTuple f (a, b) = f a b

main :: IO ()
main = do
  contents <- readStdin
  let parsed = parse $ L.words contents
  let converted = L.concatMap (mapFstOverSnds . applyToFst maskNum) parsed
  let map = M.fromList converted
  let sum = L.sum $ L.map snd $ M.toList map
  print sum
  let convertedPart2 = L.map (applyToFst masksPart2) parsed
  print $ L.map (applyToFst (applyToFst intToBin)) convertedPart2
  let dupped = L.map (applyToFst (applyToTuple dupBits)) convertedPart2
  print dupped
