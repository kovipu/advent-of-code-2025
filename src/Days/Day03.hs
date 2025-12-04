module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text (Parser, many1, digit, sepBy, endOfLine)
import Data.Void
import Debug.Trace
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
type Input = [[Char]]

inputParser :: Parser Input
inputParser = many1 digit `sepBy` endOfLine

------------ PART A ------------
partA :: Input -> Int
partA input = foldl (+) 0 largest
  where
    largest = map (read . last . sort . genPairs) input

-- generate all possible pairs
genPairs :: [Char] -> [[Char]]
genPairs [] = []
genPairs (x : xs) = pairs ++ genPairs xs
  where
    pairs = map (\(a : _) -> [x, a]) (filter (not . null) (tails xs))

------------ PART B ------------
partB :: Input -> Int
partB input = foldl (+) 0 largest
  where
    largest = map (read . findBiggest) input

-- find biggest chunk
findBiggest xs = foldl fn (take 12 xs) (drop 12 xs)

fn acc next = if b > acc then b else acc
  where
    b = deleteFirstSmaller acc ++ [next]

-- delete the first character that is smaller than its next one
deleteFirstSmaller :: (Ord a) => [a] -> [a]
deleteFirstSmaller [] = []
deleteFirstSmaller [x] = [] -- fall back to deleting the last one
deleteFirstSmaller (x : y : xs)
  | x < y = y : xs
  | otherwise = x : deleteFirstSmaller (y : xs)
