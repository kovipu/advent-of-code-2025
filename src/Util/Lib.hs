module Util.Lib where

{- ORMOLU_DISABLE -}
import Data.Maybe
import Data.List
{- ORMOLU_ENABLE -}

{-
This module contains generalized utility functions for Advent of Code problems.
-}

-- Takes a list and returns it zipped with indices starting from 0.
-- For example: enumerate "abc" = [(0,'a'),(1,'b'),(2,'c')]
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

-- Gets the neighbors of a position in a grid (8-directional).
-- Returns only the neighbors that exist within the grid bounds.
getNeighbors :: [[a]] -> Int -> Int -> [a]
getNeighbors grid x y =
  catMaybes $
    map
      ( \(x', y') ->
          grid !? (y + y') >>= (!? (x + x'))
      )
      [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

-- Replaces an element at a specific position in a 2D grid.
replace2D :: Int -> Int -> a -> [[a]] -> [[a]]
replace2D x y newVal grid =
  take y grid
    ++ [replace1D x newVal (grid !! y)]
    ++ drop (y + 1) grid

-- Replaces an element at a specific position in a list.
replace1D :: Int -> a -> [a] -> [a]
replace1D i val xs =
  take i xs ++ [val] ++ drop (i + 1) xs
