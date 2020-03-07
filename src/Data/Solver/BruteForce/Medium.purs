-- | A brute-force solver that's much faster than the Slow version because
-- | it does not compute every possible solution that could ever be made.
-- | Rather, it finds the first valid guess and continues to the next branch
-- | until the puzzle is solved, backtracking only if the current branch
-- | is invalid.
-- | It could be made faster by using a mutuable array in the underlying
-- | ArrayZipper.
module Data.Solver.BruteForce.Medium where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array ((..))
import Data.ArrayZipper (ArrayZipper, getFocus, next, prev, setFocus, shiftFocusFirst, toArrayZipperFirst)
import Data.Constraint (partialSolutionNoDiags, fullSolutionNoDiags)
import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..), fromJust)
import Data.SudokuPuzzle (CellValue(..), SudokuPuzzle)
import Matrix (set, toIndexedArray, width)
import Partial.Unsafe (unsafePartial)

type Hole = { row :: Int
            , col :: Int
            , guesses :: ArrayZipper Int
            }

findHoles :: SudokuPuzzle -> Array Hole
findHoles puzzle =
  filterMap f (toIndexedArray puzzle)
  where
    guesses = unsafePartial $ fromJust $ toArrayZipperFirst $ 1 .. (width puzzle)

    f :: { value :: CellValue, x :: Int, y :: Int } -> Maybe Hole
    f rec = case rec.value of
      Empty -> Just { row: rec.y, col: rec.x, guesses }
      _ -> Nothing

type StepState = { puzzle :: SudokuPuzzle
                 , holes :: ArrayZipper Hole
                 }

bruteForceMedium :: SudokuPuzzle -> Either StepError SudokuPuzzle
bruteForceMedium p = case toArrayZipperFirst (findHoles p) of
  Nothing -> Right p -- already solved
  Just holes -> tailRec step { puzzle:p, holes }

data StepError
  = InvalidHoleIndex
  | NoMoreHoleBackTracking Hole

instance showStepError :: Show StepError where
  show = case _ of
    InvalidHoleIndex -> "the impossible happened: we attempted to make a guess on a hole with an invalid index"
    NoMoreHoleBackTracking { row, col, guesses } ->
      "we iterated through all possibilities and could not find a valid solution. \
      \Hole was blocked at (col,row) (" <> show col <> ", " <> show row <> ") where \
      \current guess was: " <> show (getFocus guesses)

step :: StepState -> Step StepState (Either StepError SudokuPuzzle)
step { puzzle, holes } =
  let
    holeRec = getFocus holes
    guess = getFocus holeRec.guesses
  in case set holeRec.col holeRec.row (Guess guess) puzzle of
    Nothing -> Done (Left InvalidHoleIndex)
    Just updatedPuzzle ->
      if partialSolutionNoDiags updatedPuzzle
        then
          case next holes of
            Nothing ->
              if fullSolutionNoDiags updatedPuzzle
                then Done (Right updatedPuzzle)
                else backtrack puzzle holes
            Just remainingHoles ->
              Loop { puzzle: updatedPuzzle, holes: remainingHoles }
        else
          case next holeRec.guesses of
            Just nextGuess ->
              Loop { puzzle, holes: setFocus (holeRec { guesses = nextGuess }) holes }
            Nothing -> backtrack puzzle holes

resetHolesGuesses :: ArrayZipper Hole -> ArrayZipper Hole
resetHolesGuesses currentHole = resetGuessForHole
  where
  holeRec = getFocus currentHole
  resetGuesses = shiftFocusFirst holeRec.guesses
  resetGuessForHole = setFocus (holeRec { guesses = resetGuesses }) currentHole

backtrack :: SudokuPuzzle -> ArrayZipper Hole -> Step StepState (Either StepError SudokuPuzzle)
backtrack puzzle currentHole =
  case prev (resetHolesGuesses currentHole) of
      Nothing -> Done (Left (NoMoreHoleBackTracking (getFocus currentHole)))
      Just prevHole ->
        let
          prevHoleRec = getFocus prevHole
        in case set prevHoleRec.col prevHoleRec.row Empty puzzle of
          Nothing -> Done (Left InvalidHoleIndex)
          Just puzzleBeforeLastModification ->
            case next prevHoleRec.guesses of
              Nothing -> backtrack puzzleBeforeLastModification prevHole
              Just remainingGuesses ->
                let
                  updatedHole = setFocus (prevHoleRec { guesses = remainingGuesses}) prevHole
                in Loop { puzzle: puzzleBeforeLastModification, holes: updatedHole }
