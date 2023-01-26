{-# LANGUAGE TupleSections #-}

module Main where

import Data.Bifunctor as BF
import Data.Hashable
import Data.List as L
import Data.Map as M
import Data.Maybe as May
import Data.Sequence as Seq
import Data.Set as S
import Debug.Trace
import GHC.Generics (Generic)
import System.IO

readStdin :: IO String
readStdin = do
  readFile "/dev/stdin"

enumerate :: [a] -> [(Int, a)]
enumerate = L.zip [0 ..]

toInt :: String -> Int
toInt str = read str :: Int

data FIFO len a =
  FIFO len (Seq a)
  deriving (Show)

getSeq (FIFO _ seq) = seq

new :: Int -> FIFO Int a
new len = FIFO len Seq.empty

-- Never take more than the first maxLen elements from the list.
fromList :: Int -> [a] -> FIFO Int a
fromList maxLen l = FIFO maxLen (Seq.fromList limited)
  where
    limited = L.take maxLen l

dropFirst :: Seq a -> Seq a
dropFirst = Seq.deleteAt 0

-- Why the heck is there no function for converting a Seq to a list?
seqToList :: Seq a -> [a]
seqToList Seq.Empty = []
seqToList (x :<| xs) = x : rest
  where
    rest = seqToList xs

push :: a -> FIFO Int (Int, a) -> FIFO Int (Int, a)
push x (FIFO len Seq.Empty) = FIFO len (Seq.singleton (0, x))
push x (FIFO len seq) =
  if Seq.length seq < len
    then FIFO len (seq |> nextElement)
    else FIFO len (allButFirst |> nextElement)
  where
    (_ :|> (lastIdx, _)) = seq
    (_ :<| allButFirst) = seq
    nextElement = (lastIdx + 1, x)

-- Why the heck is there no Seq.map function but only mapWithIndex?
findSummable :: Int -> (Int, Int) -> Seq (Int, Int) -> [Int]
findSummable sum (idx, val) seq =
  seqToList $ Seq.mapWithIndex fstWithIndex filtered
  where
    sumFn (eIdx, e) = eIdx /= idx && sum == val + e
    filtered = Seq.filter sumFn seq
    fstWithIndex _ (idx, _) = idx

findSummablePairs :: Int -> Seq (Int, Int) -> [(Int, Int)]
findSummablePairs sum Seq.Empty = []
findSummablePairs sum (first :<| rest) =
  L.map (fst first, ) (findSummable sum first rest) ++ moreIndices
  where
    moreIndices = findSummablePairs sum rest

solvePart1 :: [Int] -> FIFO Int (Int, Int) -> [Bool]
solvePart1 [] _ = []
solvePart1 (x:xs) (FIFO len seq) = found : others
  where
    found = not $ L.null $ findSummablePairs x seq
    pushed = push x (FIFO len seq)
    others = solvePart1 xs pushed

main :: IO ()
main = do
  contents <- readStdin
  -- Read stuff in.
  let preambleLength = toInt $ L.head $ L.words contents
  let nums = L.drop 1 $ L.map toInt $ L.words contents
  let moreNums = L.drop preambleLength nums
  -- 
  let preamble = Main.fromList preambleLength $ enumerate nums
  -- let part1 = findSummable 40 (0, 25) preamble
  let part1 =
        L.map fst $
        L.filter (not . snd) $ L.zip moreNums $ solvePart1 moreNums preamble
  print part1
