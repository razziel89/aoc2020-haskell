module Main where

import System.IO
import System.IO (isEOF)
import Data.List

checkSum :: Integer
checkSum = 2020

tupleSum :: (Integer, Integer) -> Integer
tupleSum (a,b) = a * b

hasSum :: Integer -> (Integer, Integer) -> Bool
hasSum c (a,b) = (a + b) == c

pairs :: [a] -> [(a, a)]
pairs l = [ (x,y) | (x:xs) <- tails l, (y:ys) <- tails l ]

toInt :: String -> Integer
toInt s = read s :: Integer

readStdin :: IO [String]
readStdin = do
  contents <- readFile "/dev/stdin"
  return (words contents)

pr (x, [l]) = (show x, show l)

main :: IO ()
main = do
  lines <- readStdin
  let ints = map toInt lines
  let vals = pairs ints
  let pred = hasSum checkSum
  let rem = filter pred vals
  let out = head (map tupleSum rem)
  putStrLn (show out)
