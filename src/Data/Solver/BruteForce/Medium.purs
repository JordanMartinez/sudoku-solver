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
import Data.Constraint (partialSolutionNoDiags)
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

type StepState = { partialValidation :: SudokuPuzzle -> Boolean
                 , puzzle :: SudokuPuzzle
                 , holes :: ArrayZipper Hole
                 }

bruteForceMedium :: (SudokuPuzzle -> Boolean) -> SudokuPuzzle -> Either StepError SudokuPuzzle
bruteForceMedium partialValidation p = case toArrayZipperFirst (findHoles p) of
  Nothing -> Right p -- already solved
  Just holes -> tailRec step { partialValidation, puzzle:p, holes }

data StepError
  = InvalidHoleIndex
  | NoMoreHoleBackTracking

printStepError :: StepError -> String
printStepError = case _ of
  InvalidHoleIndex ->
    "the impossible happened: we attempted to make a guess on \
    \a hole with an invalid index"
  NoMoreHoleBackTracking ->
    "We iterated through all possibilities and could not find a valid solution. "

step :: StepState -> Step StepState (Either StepError SudokuPuzzle)
step { partialValidation, puzzle, holes } =
  let
    holeRec = getFocus holes
    guess = getFocus holeRec.guesses
  in case set holeRec.col holeRec.row (Guess guess) puzzle of
    Nothing -> Done (Left InvalidHoleIndex)
    Just updatedPuzzle ->
      if partialSolutionNoDiags updatedPuzzle
        then
          case next holes of
            Nothing -> Done (Right updatedPuzzle)
            Just remainingHoles ->
              Loop { partialValidation, puzzle: updatedPuzzle, holes: remainingHoles }
        else
          case next holeRec.guesses of
            Just nextGuess ->
              Loop { partialValidation, puzzle, holes: setFocus (holeRec { guesses = nextGuess }) holes }
            Nothing -> backtrack { partialValidation, puzzle, holes }

resetHolesGuesses :: ArrayZipper Hole -> ArrayZipper Hole
resetHolesGuesses currentHole = resetGuessForHole
  where
  holeRec = getFocus currentHole
  resetGuesses = shiftFocusFirst holeRec.guesses
  resetGuessForHole = setFocus (holeRec { guesses = resetGuesses }) currentHole

backtrack :: StepState -> Step StepState (Either StepError SudokuPuzzle)
backtrack { partialValidation, puzzle, holes:currentHole } =
  case prev (resetHolesGuesses currentHole) of
      Nothing -> Done (Left NoMoreHoleBackTracking)
      Just prevHole ->
        let
          prevHoleRec = getFocus prevHole
        in case set prevHoleRec.col prevHoleRec.row Empty puzzle of
          Nothing -> Done (Left InvalidHoleIndex)
          Just puzzleBeforeLastModification ->
            case next prevHoleRec.guesses of
              Nothing -> backtrack { partialValidation, puzzle: puzzleBeforeLastModification, holes: prevHole }
              Just remainingGuesses ->
                let
                  updatedHole = setFocus (prevHoleRec { guesses = remainingGuesses}) prevHole
                in Loop { partialValidation, puzzle: puzzleBeforeLastModification, holes: updatedHole }
