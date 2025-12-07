module Days.Day07 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative ((<|>))
import Data.Functor            (($>))
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
type Input = [[Cell]]
data Cell = Beam | Splitter | Empty deriving (Show, Eq)
inputParser :: Parser Input
inputParser = parseLine `sepBy` endOfLine
  where
    parseLine = many1 $ parseStart <|> parseEmpty <|> parseSplitter
    parseStart = char 'S' $> Beam
    parseEmpty = char '.' $> Empty
    parseSplitter = char '^' $> Splitter

------------ PART A ------------
partA :: Input -> Int
partA input = fst $ foldl fn (0, start) rest
  where
    (start:rest) = input
    fn (splits, state) line = (s, state')
      where
        (state', _, s) = foldr fn' ([], False, splits) (zip state line)
    fn' (prev, next) (acc, isSplitting, splits') =
      -- ignore the case of two Splitters next to each other. Does not happen in the input data.
      case (prev, next, isSplitting) of
        -- isSplitting true -> create beam here
        (Empty, _, True) -> (Beam : acc, False, splits')
        -- issplitting false -> empty
        (Empty, _, False) -> (Empty : acc, False, splits')
        -- split
        (Beam, Splitter, _) -> (Splitter : Beam : (drop 1 acc), True, splits' + 1)
        -- continue the beam
        (Beam, _, _) -> (Beam : acc, False, splits')
        -- splitters can never have a beam right below them
        (Splitter, _, _) -> (Empty : acc, False, splits')

------------ PART B ------------
partB :: Input -> Void
partB = error "Not implemented yet!"
