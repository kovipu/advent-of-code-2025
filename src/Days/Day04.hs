module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative ((<|>))
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U
import Util.Lib (enumerate, getNeighbors, replace2D, replace1D)

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text (Parser, many1, char, sepBy, endOfLine)
import Data.Void
import Debug.Trace
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
type Input = [[Char]]

inputParser :: Parser Input
inputParser = many1 (char '.' <|> char '@') `sepBy` endOfLine

------------ PART A ------------
partA :: Input -> Int
partA input =
  foldl
    ( \acc (y, line) ->
        ( foldl
            ( \acc' (x, c) ->
                let neighbors = getNeighbors input x y
                    numRolls = length $ filter ('@' ==) neighbors
                 in if c == '@' && numRolls < 4 then acc' + 1 else acc'
            )
            acc
            (enumerate line)
        )
    )
    0
    (enumerate input)

------------ PART B ------------
partB :: Input -> Int
partB input = deletPapers input 0

-- recurse and remove more rolls until no more can be removed.
deletPapers :: Input -> Int -> Int
deletPapers state n =
  if newState == state then n' else deletPapers newState n'
  where
    (newState, n') =
      foldl
        ( \acc (y, line) ->
            foldl
              ( \(st, nt) (x, c) ->
                  let -- tiny optimization to use st instead of state here
                      neighbors = getNeighbors st x y
                      numRolls = length $ filter ('@' ==) neighbors
                   in if c == '@' && numRolls < 4 then (removeRoll x y st, nt + 1) else (st, nt)
              )
              acc
              (enumerate line)
        )
        (state, n)
        (enumerate state)
    removeRoll x y = replace2D x y '.'
