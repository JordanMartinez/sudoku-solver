-- | An extroardinarily inefficient solver that will eventually terminate.
-- | The entire sudoku puzzle is validated every time rather than only
-- | validating the guess that was made.
module Data.Solver.BruteForce.Slow where

import Prelude

import Data.Array ((..))
import Data.Zipper.ArrayZipper (ArrayZipper, getFocus, next, toArrayZipperFirst)
import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..))
import Data.SudokuPuzzle (CellValue(..), SudokuPuzzle)
import Matrix (set, toIndexedArray, width)

type Hole = { row :: Int
            , col :: Int
            }

findHoles :: SudokuPuzzle -> Array Hole
findHoles puzzle =
  filterMap f (toIndexedArray puzzle)
  where
    f :: { value :: CellValue, x :: Int, y :: Int } -> Maybe Hole
    f rec = case rec.value of
      Empty -> Just { row: rec.x, col: rec.y }
      _ -> Nothing

bruteForceSlow :: (SudokuPuzzle -> Boolean) -> SudokuPuzzle -> Array SudokuPuzzle
bruteForceSlow validateSolution puzzle =
  let
    guesses = 1 .. (width puzzle)
    mbZipper = toArrayZipperFirst (findHoles puzzle)
  in case mbZipper of
    Nothing -> [puzzle] -- already solved
    Just holes -> loop validateSolution guesses puzzle holes

loop :: (SudokuPuzzle -> Boolean) -> Array Int -> SudokuPuzzle -> ArrayZipper Hole -> Array SudokuPuzzle
loop validateSolution guesses currentPuzzle currentHole = do
  let { row, col } = getFocus currentHole
  guess <- guesses
  let modificationResult = set col row (Guess guess) currentPuzzle
  case modificationResult of
    Nothing -> []
    Just updatedPuzzle -> do
      if (validateSolution updatedPuzzle)
        then case next currentHole of
          Nothing -> pure updatedPuzzle
          Just nextHole -> loop validateSolution guesses updatedPuzzle nextHole
        else
          []
