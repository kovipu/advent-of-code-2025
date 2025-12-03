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
import Data.Attoparsec.Text
import Data.Void
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
genPairs []     = []
genPairs (x:xs) = pairs ++ genPairs xs
  where
    pairs = map (\(a:_) -> [x, a]) (filter (not . null) (tails xs))

------------ PART B ------------
partB :: Input -> Int
partB input = foldl (+) 0 largest
  where
    largest = map (read . last . sort . genDozens) input

--  generate all possible chunks of 12
genDozens :: [Char] -> [[Char]]
genDozens xs = filter (\x -> length x == 12) $ subsequences xs
