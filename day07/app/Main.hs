{-# LANGUAGE TupleSections #-}

module Main where

import Data.Bifunctor as BF
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

noTo :: String -> String -> String
noTo def "no" = "0"
noTo def s = s

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

-- This type was a rather stupid decision. But we have some code that's using it
-- so we're keeping it for this puzzle.
data Def =
  Def Bag (Map Bag Int)
  deriving (Show, Ord, Eq)

getBag :: Def -> Bag
getBag (Def b m) = b

getBags :: Def -> [Bag]
getBags (Def b m) = M.keys m

defsToBagMap :: [Def] -> Map Bag [(Bag, Int)]
defsToBagMap l = M.fromList $ L.map (convert . extract) l
  where
    getBagsWithCounts :: Def -> [(Bag, Int)]
    getBagsWithCounts (Def b m) = M.toList m
    --
    extract :: Def -> (Bag, [(Bag, Int)])
    extract d = (getBag d, getBagsWithCounts d) -- :: Def -> (Bag, [(Bag, Int)])
    -- Make sure that bags that do not contain any other bags will be mapped to
    -- something that we can then count. This is pretty hacky, admittedly.
    convert :: (Bag, [(Bag, Int)]) -> (Bag, [(Bag, Int)])
    convert (b, []) = (b, [(plain, 1)])
    convert (b, l) = (b, l)

parse :: (String -> String) -> String -> Def
parse makeNoAnInt l = Def bag map
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
           , toInt $ makeNoAnInt $ applyToWords (L.take 1) s))
        secondHalf

shiny :: Bag
shiny = Bag "shiny gold"

plain :: Bag
plain = Bag "plain"

plainDef :: Def
plainDef = Def plain plainMap
  where
    plainMap = M.fromList [(plain, 1)]

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

multiply :: [(Int, [(a, Int)])] -> [(a, Int)]
multiply = L.concatMap conv
  where
    conv :: (Int, [(a, Int)]) -> [(a, Int)]
    conv (count, l) = L.map (BF.second (count *)) l

accum :: Map Bag [(Bag, Int)] -> (Int, Map Bag Int) -> (Int, Map Bag Int)
accum bm (acc, m) =
  case M.size m of
    0 -> error "empty map detected"
    1 ->
      if M.member shiny m
        then nextCall
        else (more, m)
    _ -> nextCall
  where
    converted :: [(Int, [(Bag, Int)])]
    converted = L.map (\e -> (snd e, lookupWithPanic bm $ fst e)) $ M.toList m
    --
    convertedMap = M.fromListWith (+) $ multiply converted
    --
    noPlain = M.filterWithKey (\k v -> k /= plain) convertedMap
    more = acc + L.sum (M.elems noPlain)
    --
    nextCall = accum bm (more, convertedMap)

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
  let defs = shinyRemains $ makeMap $ L.map (parse (noTo "0")) $ lines contents
  let bags = L.filter (/= shiny) $ M.keys defs
  let part1 =
        L.length $ L.filter (== [shiny]) $ L.map (\b -> translate defs [b]) bags
  print part1
  -- Part 2.
  -- Def Bag (Map Bag Int)
  let defsPart2 = plainDef : L.map (parse (noTo "0")) (lines contents)
  let part2 = accum (defsToBagMap defsPart2) (0, M.fromList [(shiny, 1)])
  print $ fst part2
