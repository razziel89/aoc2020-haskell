{-# LANGUAGE TupleSections #-}

module Main where

import Data.Hashable
import Data.List as L
import Data.Map as M
import Data.Set as S
import Debug.Trace
import GHC.Generics (Generic)
import System.IO

readStdin :: IO String
readStdin = do
  readFile "/dev/stdin"

toInt :: String -> Int
toInt str = read str :: Int

noToZero :: String -> String
noToZero "no" = "0"
noToZero s = s

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn pred input =
  case dropWhile pred input of
    [] -> []
    l -> word : splitOn pred rest
      where (word, rest) = break pred l

count :: (Eq a) => a -> [a] -> Int
count val l = L.length $ L.filter (== val) l

elemCounts :: (Ord a) => [[a]] -> Map a Int
elemCounts ll = M.fromListWith (+) $ L.map (, 1) $ concat ll

applyToWords :: ([String] -> [String]) -> String -> String
applyToWords f s = L.unwords $ f $ L.words s

newtype Bag =
  Bag String
  deriving (Show, Ord, Eq)

data Def =
  Def Bag (Map Bag Int)
  deriving (Show, Ord, Eq)

getBag :: Def -> Bag
getBag (Def b m) = b

getBags :: Def -> [Bag]
getBags (Def b m) = M.keys m

parse :: String -> Def
parse l = Def bag map
  where
    line = L.filter (/= '.') l
    bag = Bag $ applyToWords (L.take 2) line
    secondHalf =
      L.map (applyToWords $ L.take 3) $
      splitOn (== ',') $ applyToWords (L.drop 4) line
    map =
      M.fromList $
      L.filter ((/= 0) . snd) $
      L.map
        (\s ->
           ( Bag $ applyToWords (L.drop 1) s
           , toInt $ noToZero $ applyToWords (L.take 1) s))
        secondHalf

shiny :: Bag
shiny = Bag "shiny gold"

emptyBag :: Bag
emptyBag = Bag ""

makeUnique :: (Ord a) => [a] -> [a]
makeUnique = S.toList . S.fromList

makeMap :: [Def] -> Map Bag [Bag]
makeMap l = M.fromList $ L.map (\d -> (getBag d, getBags d)) l

lookupWithPanic :: (Ord k) => Map k [v] -> k -> [v]
lookupWithPanic m k =
  if M.member k m
    then M.findWithDefault [] k m
    else error "cannot find bag"

myTrace :: (Show a) => a -> a
myTrace a = traceShow a a

translate :: Map Bag [Bag] -> [Bag] -> [Bag]
translate m l =
  case L.length l of
    0 -> l
    1 ->
      if L.head l == shiny
        then l
        else newlist
    _ -> newlist
  where
    newlist = translate m $ makeUnique $ L.concatMap (lookupWithPanic m) l

shinyRemains :: Map Bag [Bag] -> Map Bag [Bag]
shinyRemains = M.mapWithKey f
  where
    f k v =
      if k == shiny
        then [shiny]
        else v

main :: IO ()
main = do
  contents <- readStdin
  -- Part 1.
  -- Filter out the shiny bag as key because we do not want to traverse that
  -- bag. At least not for part 1.
  let defs = shinyRemains $ makeMap $ L.map parse $ lines contents
  print defs
  let bags = L.filter (/= shiny) $ M.keys defs
  let part1 =
        L.length $ L.filter (== [shiny]) $ L.map (\b -> translate defs [b]) bags
  print part1
