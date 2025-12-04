module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative ((<|>))
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U
import           Data.Functor            (($>))
import Debug.Trace

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseLine `sepBy` endOfLine

parseLine :: Parser (Inst)
parseLine = do
  dir <- char 'L' $> L <|> char 'R' $> R
  n <- decimal
  pure $ (dir, n)

------------ TYPES ------------
data Dir = L | R deriving (Eq, Show)

type Inst = (Dir, Int)

type Input = [Inst]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input = snd $ foldl fn (50, 0) input

fn (pos, zeros) (dir, n) = (pos', zeros')
  where
    n' = n `mod` 100
    p = if dir == L then pos - n' else pos + n'
    pos'
      | p < 0 = p + 100
      | p >= 100 = p - 100
      | otherwise = p
    zeros' = if pos' == 0 then zeros + 1 else zeros

------------ PART B ------------
partB :: Input -> OutputB
partB input = snd $ foldl fn' (50, 0) input

fn' (pos, zeros) (dir, n) = (pos', zeros' + loops)
  where
    n' = n `mod` 100
    -- every full loop causes the dial to point at 0 once
    loops = n `div` 100
    p = if dir == L then pos - n' else pos + n'
    (pos', zeros')
      | p == 0 = (p, zeros + 1)
      | p < 0 = (p + 100, if pos /= 0 then zeros + 1 else zeros)
      | p >= 100 = (p - 100, if pos /= 0 then zeros + 1 else zeros)
      | otherwise = (p, zeros)
