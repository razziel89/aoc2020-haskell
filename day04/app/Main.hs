{-# LANGUAGE
    DeriveGeneric
 #-}
 module Main where

import System.IO
import System.IO (isEOF)
import Data.List as L
import Data.Map as M
import Data.Set as S
import GHC.Generics (Generic)
import Data.Hashable

readStdin :: IO String
readStdin = do
  contents <- readFile "/dev/stdin"
  return contents

enumerate :: [a] -> [(Int, a)]
enumerate l = zip [0..] l

toInt :: String -> Int
toInt str = read str :: Int

-- splitOn :: (Char -> Bool) -> String -> [String]
-- splitOn pred inputStr =
--   case dropWhile pred inputStr of
--     "" -> []
--     str -> word : (splitOn pred rest)
--       where (word, rest) = break pred str

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn pred input =
  case dropWhile pred input of
    [] -> []
    l -> word : (splitOn pred rest)
      where (word, rest) = break pred l

passportToFields :: [String] -> [String]
passportToFields p = L.map head $ L.map (splitOn (==':')) p

reqFields =
  [ "byr"
  , "iyr"
  , "eyr"
  , "hgt"
  , "hcl"
  , "ecl"
  , "pid"
  , "cid" ]

hasElem :: (Eq a) => a -> [a] -> Bool
hasElem e [] = False
hasElem e (x:xs) = if x == e then True else hasElem e xs

main :: IO ()
main = do
  contents <- readStdin
  -- Split into lines and create nested lists that are each separated by
  -- empty lines.
  let split = splitOn (=="") $ lines contents
  -- Split each string into its component words but then combine all lists again
  -- so that each passport is a single entry.
  let passports = L.map concat $ L.map (L.map words) split
  let passportFields = L.map S.fromList $ L.map (L.filter (/="cid")) $ L.map passportToFields passports
  let fields = S.fromList $ L.filter (/="cid") reqFields
  -- let valsPart1 = length $ L.filter (==True) $ L.map (==fields) passportFields
  let valsPart1 = length $ L.filter (==True) $ L.map (==fields) passportFields
  putStrLn (show valsPart1)
  -- let valsPart2 = L.map (countTrees contents) part2Directions
  -- putStrLn (show $ L.product valsPart2)
