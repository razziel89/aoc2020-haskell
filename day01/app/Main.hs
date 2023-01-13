module Main where

import System.IO
import System.IO (isEOF)
import Data.List

readStdin :: IO [String]
readStdin = do
  contents <- readFile "/dev/stdin"
  return (words contents)

checkSum = 2020 :: Integer

hasSum :: Integer -> [Integer] -> Bool
hasSum a b =  (sum b) == a

toPairs :: [a] -> [[a]]
toPairs l = [ [x,y] | (x:xs) <- tails l, (y:ys) <- tails l ]

toTriplets :: [a] -> [[a]]
toTriplets l = [ [x,y,z] | (x:xs) <- tails l, (y:ys) <- tails l, (z:zs) <- tails l ]

toInt :: String -> Integer
toInt s = read s :: Integer

solve :: ([Integer] -> [[Integer]]) -> [String] -> Integer
solve conv l = foldl (*) 1 $ head $ valid
  where 
    combined = conv $ map toInt l
    valid = filter (hasSum checkSum) combined

main :: IO ()
main = do
  lines <- readStdin
  let part1 = solve toPairs lines
  putStrLn (show part1)
  let part2 = solve toTriplets lines
  putStrLn (show part2)
