module Days.Day02 (runDay) where

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
import Data.Attoparsec.Text (Parser, sepBy, decimal, char)
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parsePair `sepBy` char ','

parsePair = do
  a <- decimal
  char '-'
  b <- decimal
  pure (a, b)

------------ TYPES ------------
type Input = [(Int, Int)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = foldl fn 0

fn acc (a, b) = foldl (+) acc $ filter isPasswordInvalid [a..b]

isPasswordInvalid n = start == end
  where
    s = show n
    l = length s
    (start, end) = splitAt (l `div` 2) s


------------ PART B ------------
partB :: Input -> OutputB
partB = foldl fn' 0

fn' acc (a, b) = foldl (+) acc $ filter isPasswordInvalid' [a..b]

isPasswordInvalid' n = any (\k -> isChunkMatch k s) [1..(l - 1)]
  where
    s = show n
    l = length s

    -- do all of the members of that group match?
    isChunkMatch k s = all (== head chunks) chunks
      where
        chunks = equalChunks k s

    -- generate chunks with lengths 1..(length p)
    equalChunks _ [] = []
    equalChunks k xs = take k xs : equalChunks k (drop k xs)



