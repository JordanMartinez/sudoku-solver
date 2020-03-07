module Main where

import Prelude

import Data.Constraint (partialSolutionNoDiags)
import Data.Either (Either(..))
import Data.Monoid (power)
import Data.Solver.BruteForce.Medium (bruteForceMedium)
import Data.SudokuPuzzle (SudokuPuzzle, puzzle2x2, puzzle9x9, puzzleSolved)
import Effect (Effect)
import Effect.Console (log)
import Matrix (width)

main :: Effect Unit
main = do
  printPuzzleAndSolution "Solving already solved puzzle: 2x2" puzzleSolved
  printPuzzleAndSolution "Solving puzzle: 2x2" puzzle2x2
  printPuzzleAndSolution "Solving puzzle: 9x9" puzzle9x9

printPuzzleAndSolution :: String -> SudokuPuzzle -> Effect Unit
printPuzzleAndSolution msg puzzle = do
  log msg
  log $ show puzzle
  log $ power "=" (((width puzzle) * 3) + ((width puzzle) * 2) - 2)
  case bruteForceMedium partialSolutionNoDiags puzzle of
    Left e -> log $ show $ "Could not solve puzzle: " <> show e
    Right p -> log $ show p
  log "\n\n"
