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

solution :: ((Int, Int), Char, String) -> Bool
solution ((rStart, rEnd), c, s) = count >= rStart && count <= rEnd
  where
    count = length $ filter (== c) s

solve :: [String] -> Int
solve l = length $ filter solution $ map parse $ map (splitOn (== ' ')) l

main :: IO ()
main = do
  lines <- readStdin
  let out = solve lines
  putStrLn (show out)
  -- let part1 = solve lines
  -- putStrLn (show part1)
  -- let part2 = solve toTriplets lines
  -- putStrLn (show part2)
