module Main where

import System.IO
import System.IO (isEOF)
import Data.List

readStdin :: IO [String]
readStdin = do
  contents <- readFile "/dev/stdin"
  return (lines contents)

toInt :: String -> Integer
toInt str = read str :: Integer

splitOn :: (Char -> Bool) -> String -> [String]
splitOn pred inStr =
  case dropWhile pred inStr of
    "" -> []
    str -> w : splitOn pred broken
      where (w, broken) = break pred str

solve :: [String] -> [[String]]
solve l = map (splitOn (== ' ')) l

main :: IO ()
main = do
  lines <- readStdin
  let out = solve lines
  putStrLn (show out)
  -- let part1 = solve lines
  -- putStrLn (show part1)
  -- let part2 = solve toTriplets lines
  -- putStrLn (show part2)
