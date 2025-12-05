module Days.Day05 (runDay) where

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
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
data Input = Input
  { ranges :: [(Int, Int)],
    ingredients :: [Int]
  }
  deriving (Show)

inputParser :: Parser Input
inputParser = do
  ranges <- parseRange `sepBy` endOfLine
  many1 space
  ingredients <- decimal `sepBy` endOfLine
  pure $ Input {ranges, ingredients}

parseRange = do
  a <- decimal
  char '-'
  b <- decimal
  pure (a, b)

------------ PART A ------------
partA :: Input -> Int
partA Input {ranges, ingredients} =
  foldl
    ( \acc ig ->
        if any (\(a, b) -> a <= ig && ig <= b) ranges
          then acc + 1
          else acc
    )
    0
    ingredients

------------ PART B ------------
partB :: Input -> Int
partB Input {ranges} = sum $ map (\(a, b) -> b - a + 1) $ mergeOverlapping ranges

-- if any of the ranges overlap -> join them together -> recurse until no change
mergeOverlapping ranges =
  if ranges == ranges' then ranges' else mergeOverlapping ranges'
  where
    ranges' = foldl mergeRange [] ranges

mergeRange [] r = [r]
mergeRange ((x, y) : xs) (a, b) =
  if max x a <= min y b
    -- if ranges overlap -> combine to give the biggest range
    then (min a x, max b y) : xs
    -- otherwise tail recurse
    else (x, y) : (mergeRange xs (a, b))
