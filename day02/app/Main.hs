module Main where

import System.IO
import System.IO (isEOF)
import Data.List

readStdin :: IO [String]
readStdin = do
  contents <- readFile "/dev/stdin"
  return $ lines contents

toInt :: String -> Int
toInt str = read str :: Int

splitOn :: (Char -> Bool) -> String -> [String]
splitOn pred inputStr =
  case dropWhile pred inputStr of
    "" -> []
    str -> word : (splitOn pred rest)
      where (word, rest) = break pred str

parseRange :: String -> (Int, Int)
parseRange rangeStr = (firstEl, lastEl)
  where
    firstEl = head els
    lastEl = last els
    els = map toInt $ splitOn (== '-') rangeStr

parse :: [String] -> ((Int, Int), Char, String)
parse l = ((parseRange range), char, passwd)
  where
    range = head l
    char = head $ head $ drop 1 l
    passwd = last l

solutionPart1 :: ((Int, Int), Char, String) -> Bool
solutionPart1 ((rStart, rEnd), c, s) = count >= rStart && count <= rEnd
  where
    count = length $ filter (== c) s

solutionPart2 :: ((Int, Int), Char, String) -> Bool
solutionPart2 ((leftPos, rightPos), c, s) = 
  (c == left && c /= right) ||  (c /= left && c == right)
  where
    left = head $ drop (leftPos - 1) s
    right = head $ drop (rightPos - 1) s

solve :: (((Int, Int), Char, String) -> Bool) -> [String] -> Int
solve solution l = length $ filter solution $ map parse $ map (splitOn (== ' ')) l

main :: IO ()
main = do
  lines <- readStdin
  let part1 = solve solutionPart1 lines
  putStrLn (show part1)
  let part2 = solve solutionPart2 lines
  putStrLn (show part2)
