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

reqFieldsList =
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

validPart1 :: Set String -> [String] -> Bool
validPart1 reqSet split = fields == reqSet
  where
  -- Extract fields but disregard the one that doesn't matter for the
  -- comparison.
    fields = S.fromList $ L.filter (/="cid") $ passportToFields passport
    -- Split each string into its component words but then combine all lists again
    -- so that each passport is a single entry.
    passport = concat $ L.map words split
    

main :: IO ()
main = do
  contents <- readStdin
  -- Split into lines and create nested lists that are each separated by empty
  -- lines.
  let split = splitOn (=="") $ lines contents
  -- Get set of those fields that are required.
  let reqFieldsSet = S.fromList $ L.filter (/="cid") reqFieldsList
  -- Check which passport is valid for part 1.
  let valsPart1 = length $ L.filter (\x -> x) $ L.map (validPart1 reqFieldsSet) split
  putStrLn (show valsPart1)
  -- let valsPart2 = L.map (countTrees contents) part2Directions
  -- putStrLn (show $ L.product valsPart2)
