module Days.Day06 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative ((<|>), many, optional)
import           Data.Functor            (($>))
import Data.List
import Data.List.Split (splitWhen)
import Data.Char
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
type Input = ([String], [Op])

data Op = Add | Mul deriving (Show, Eq)

inputParser :: Parser Input
inputParser = do
  nums <- count 4 parseLine
  ops <- parseOp `sepBy` many' space
  pure (nums, ops)
  where
    parseOp = char '*' $> Mul <|> char '+' $> Add
    parseLine = do
      ts <- many1 $ notChar '\n'
      endOfLine
      pure ts

------------ PART A ------------
-- Changed the Input data & Parser for part B so the part A code broke.
partA :: Input -> Int
partA inp = 0

-- partA (nums, ops) =
--   foldl
--     ( \acc (op, ns) ->
--         if op == Mul
--           then acc + foldl (*) 1 ns
--           else acc + sum ns
--     )
--     0
--     (zip ops $ transpose nums)

------------ PART B ------------
partB :: Input -> Int
partB (nums, ops) =
  foldl
    ( \acc (op, ns) ->
        if op == Mul
          then acc + foldl (\acc n -> (read n) * acc) 1 ns
          else acc + foldl (\acc n -> (read n) + acc) 0 ns
    )
    0
    (zip ops (splitWhen (== "    ") $ transpose nums))
