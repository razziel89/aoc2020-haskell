module Main where

import System.IO
import System.IO (isEOF)
import Data.List

checkSum :: Integer
checkSum = 2020

tupleProd :: (Integer, Integer) -> Integer
tupleProd (a,b) = a * b

tupleSum :: (Integer, Integer) -> Integer
tupleSum (a,b) = a + b

hasSum :: Integer -> (Integer, Integer) -> Bool
hasSum a b =  (tupleSum b) == a

toPairs :: [a] -> [(a, a)]
toPairs l = [ (x,y) | (x:xs) <- tails l, (y:ys) <- tails l ]

toInt :: String -> Integer
toInt s = read s :: Integer

readStdin :: IO [String]
readStdin = do
  contents <- readFile "/dev/stdin"
  return (words contents)

pr (x, [l]) = (show x, show l)

solvePart1 :: [String] -> Integer
solvePart1 l = tupleProd $ head $ valid
  where 
    pairs = toPairs $ map toInt l
    valid = filter (hasSum checkSum) pairs

main :: IO ()
main = do
  lines <- readStdin
  let out = solvePart1 lines
  putStrLn (show out)
