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

inRange :: (Ord a) => a -> a -> a -> Bool
inRange min max val = min <= val && val <= max

canBeInt :: String -> Bool
canBeInt l = L.all (inRange '0' '9')  l

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn pred input =
  case dropWhile pred input of
    [] -> []
    l -> word : (splitOn pred rest)
      where (word, rest) = break pred l

passportToFields :: [String] -> [(String, String)]
passportToFields p = L.map (\x -> (head x, last x)) $ L.map (splitOn (==':')) p

reqFieldsList =
  [ "byr"
  , "iyr"
  , "eyr"
  , "hgt"
  , "hcl"
  , "ecl"
  , "pid"
  , "cid" ]

hasLength :: Int -> String -> Bool
hasLength len val = (length val) == len

isEyeCol :: String -> Bool
isEyeCol "amb" = True
isEyeCol "blu" = True
isEyeCol "brn" = True
isEyeCol "gry" = True
isEyeCol "grn" = True
isEyeCol "hzl" = True
isEyeCol "oth" = True
isEyeCol _ = False

isHairCol :: String -> Bool
isHairCol (x:xs) = x == '#' && lenCheck && isCol
  where
    lenCheck = hasLength 6 xs
    isCol = L.all (\x -> (inRange 'a' 'f' x) || (inRange '0' '9' x)) xs

reverseList :: [a] -> [a]
reverseList xs = L.foldl (\x y -> y:x) [] xs 

endsWith :: (Eq a) => [a] -> [a] -> Bool
endsWith ext l = L.all (\entry -> (fst entry) == (snd entry)) $ zip revExt revL
  where
    revExt = reverseList ext
    revL = reverseList l

isHeight :: String -> Bool
isHeight s = (endsWith "in" s && inRange 59 76 num) || (endsWith "cm" s && inRange 150 193 num)
  where
    num = toInt $ reverseList $ L.drop 2 $ reverseList s

-- Comments about validity for all fields:
--
-- byr (Birth Year) - four digits; at least 1920 and at most 2002.
-- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
-- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
-- hgt (Height) - a number followed by either cm or in:
--     If cm, the number must be at least 150 and at most 193.
--     If in, the number must be at least 59 and at most 76.
-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
-- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
-- pid (Passport ID) - a nine-digit number, including leading zeroes.
-- cid (Country ID) - ignored, missing or not.

reqFieldsRules :: [(String, (String -> Bool))]
reqFieldsRules=
  [
  ("byr", (\s -> inRange 1920 2002 $ toInt s)) ,
  ("iyr", (\s -> inRange 2010 2020 $ toInt s)) ,
  ("eyr", (\s -> (hasLength 4 s) && (inRange 2020 2030 $ toInt s))) ,
  ("hgt", (\s -> isHeight s)) ,
  ("hcl", (\s -> isHairCol s)) ,
  ("ecl", (\s -> isEyeCol s)) ,
  ("pid", (\s -> hasLength 9 s && canBeInt s)) ,
  ("cid", (\s -> True))
  ]

hasElem :: (Eq a) => a -> [a] -> Bool
hasElem e [] = False
hasElem e (x:xs) = if x == e then True else hasElem e xs

validPart1 :: Set String -> [String] -> Bool
validPart1 reqSet passport = (S.fromList $ M.keys fields) == reqSet
  where
  -- Extract fields but disregard the one that doesn't matter for the
  -- comparison.
    fields = M.fromList $ L.filter (\x -> (fst x) /= "cid") $ passportToFields passport

validPart2 :: Set String -> Map String (String -> Bool) -> [String] -> Bool
validPart2 reqSet ruleMap passport =
  (validPart1 reqSet passport) && L.all (\entry -> (snd entry) (fst entry)) valsAndChecks
  where
    fields = M.fromList $ passportToFields passport
    defaultOp = (\x -> False) :: (String -> Bool)
    valsAndChecks = M.elems $ M.intersectionWith (\x y -> (x, y)) fields ruleMap :: [(String, (String -> Bool))]

main :: IO ()
main = do
  contents <- readStdin
  -- Extract passports from weird input format.
  let passports = L.map (\x -> concat $ L.map words x) $ splitOn (=="") $ lines contents
  -- Get set of those fields that are required.
  let reqFieldsSet = S.fromList $ L.filter (/="cid") reqFieldsList
  -- Check which passport is valid for part 1.
  let valsPart1 = length $ L.filter (==True) $ L.map (validPart1 reqFieldsSet) passports
  putStrLn (show valsPart1)
  -- part 2
  let rulesMap = M.fromList reqFieldsRules
  let valsPart2 = length $ L.filter (==True) $ L.map (validPart2 reqFieldsSet rulesMap) passports
  putStrLn (show valsPart2)
  -- let valsPart2 = L.map (countTrees contents) part2Directions
  -- putStrLn (show $ L.product valsPart2)
