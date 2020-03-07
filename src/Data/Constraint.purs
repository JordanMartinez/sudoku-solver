module Data.Constraint where

import Prelude

import Data.Array ((..))
import Data.Filterable (filterMap)
import Data.Foldable (foldl, all)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (floor, quot, toNumber)
import Data.List (List(..), null, reverse, (:))
import Data.Map (Map, insertWith)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.SudokuPuzzle (CellValue(..), SudokuPuzzle, isEmptyCell)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Math (sqrt)
import Matrix (columns, get, getColumn, getRow, rows, width)

-- | Indicates a row index in the SudokuPuzzle
newtype RowIndex = RowIndex Int
derive instance newtypeRowIndex :: Newtype RowIndex _

-- | Indicates a column index in the SudokuPuzzle
newtype ColumnIndex = ColumnIndex Int
derive instance newtypeColumn :: Newtype ColumnIndex _

type Duplicate = { number :: Int, count :: Int}

-- | Returns an empty list if the array is empty or does not contain duplicates.
uniqueArray :: Array Int -> List Duplicate
uniqueArray = case _ of
  [] -> Nil
  array ->
    let
      countAppearances :: Map Int Int -> Int -> Map Int Int
      countAppearances map next = insertWith (\count _ -> count + 1) next 1 map

      countResult :: Map Int Int
      countResult = foldl countAppearances M.empty array

      accumulateDuplicates :: Int -> List Duplicate -> Int -> List Duplicate
      accumulateDuplicates key acc value
        | value == 1 = acc
        | otherwise = {number: key, count: value} : acc
    in
      reverse $ foldlWithIndex accumulateDuplicates Nil countResult

-- Used in `filterMap` function calls later in this file.
extractAndKeepInts :: CellValue -> Maybe Int
extractAndKeepInts = case _ of
  Original i -> Just i
  Guess i -> Just i
  _ -> Nothing

uniqueRow :: RowIndex -> SudokuPuzzle -> Maybe (List Duplicate)
uniqueRow (RowIndex idx) puzzle = do
  row <- getRow idx puzzle
  let realValues = filterMap extractAndKeepInts row
  pure (uniqueArray realValues)

uniqueColumn :: ColumnIndex -> SudokuPuzzle -> Maybe (List Duplicate)
uniqueColumn (ColumnIndex idx) puzzle = do
  column <- getColumn idx puzzle
  let realValues = filterMap extractAndKeepInts column
  pure (uniqueArray realValues)

uniqueIndices :: Array (Tuple RowIndex ColumnIndex) -> SudokuPuzzle -> Maybe (List Duplicate)
uniqueIndices indexArray puzzle =
  pure $ uniqueArray $ filterMap puzzleCellsExtractedAsInts indexArray
  where
    puzzleCellsExtractedAsInts :: Tuple RowIndex ColumnIndex -> Maybe Int
    puzzleCellsExtractedAsInts = case _ of
      Tuple (RowIndex rowIdx) (ColumnIndex colIdx) -> do
        value <- get colIdx rowIdx puzzle
        extractAndKeepInts value

uniqueGrid :: Tuple RowIndex ColumnIndex -> SudokuPuzzle -> Maybe (List Duplicate)
uniqueGrid (Tuple (RowIndex row) (ColumnIndex col)) puzzle =
  let
    gridSize = floor (sqrt (toNumber (width puzzle))) -- square root
    rowStart = (row `quot` gridSize) * gridSize
    rowEnd = rowStart + gridSize - 1
    colStart = (col `quot` gridSize) * gridSize
    colEnd = colStart + gridSize - 1

    indices :: Array (Tuple RowIndex ColumnIndex)
    indices = do
      r <- rowStart .. rowEnd
      c <- colStart .. colEnd
      pure (Tuple (RowIndex r) (ColumnIndex c))
  in uniqueIndices indices puzzle

uniqueDiagonalTopLBottomR :: SudokuPuzzle -> Maybe (List Duplicate)
uniqueDiagonalTopLBottomR puzzle =
  uniqueIndices indexArray puzzle
  where
    indexArray = (0 .. ((width puzzle) - 1)) <#> (\i -> Tuple (RowIndex i) (ColumnIndex i))

uniqueDiagonalTopRBottomL :: SudokuPuzzle -> Maybe (List Duplicate)
uniqueDiagonalTopRBottomL puzzle =
  uniqueIndices indexArray puzzle
  where
    lastIndex = width puzzle - 1
    indexArray = (0 .. lastIndex) <#> (\i -> Tuple (RowIndex i) (ColumnIndex (lastIndex - i)))

noEmptyCells :: SudokuPuzzle -> Boolean
noEmptyCells = all (not <<< isEmptyCell)

noDuplicatesFound :: Array CellValue -> Boolean
noDuplicatesFound array = null (uniqueArray (filterMap extractAndKeepInts array))

partialAllRowsValid :: SudokuPuzzle -> Boolean
partialAllRowsValid = all noDuplicatesFound <<< rows

partialAllColumnsValid :: SudokuPuzzle -> Boolean
partialAllColumnsValid = all noDuplicatesFound <<< columns

partialAllGridsValid :: SudokuPuzzle -> Boolean
partialAllGridsValid puzzle = all (\g -> (Just Nil) == (uniqueGrid g puzzle)) grids
  where
    gridSize = floor (sqrt (toNumber (width puzzle)))

    grids :: Array (Tuple RowIndex ColumnIndex)
    grids = do
      let array = map (_ * gridSize) (0 .. (gridSize - 1))
      r <- array
      c <- array
      pure (Tuple (RowIndex r) (ColumnIndex c))

partialTopLBottomR :: SudokuPuzzle -> Boolean
partialTopLBottomR puzzle = uniqueDiagonalTopLBottomR puzzle == Just Nil

partialTopRBottomL :: SudokuPuzzle -> Boolean
partialTopRBottomL puzzle = uniqueDiagonalTopRBottomL puzzle == Just Nil

fullSolutionNoDiags :: SudokuPuzzle -> Boolean
fullSolutionNoDiags = noEmptyCells && partialSolutionNoDiags

partialSolutionNoDiags :: SudokuPuzzle -> Boolean
partialSolutionNoDiags =
  partialAllRowsValid && partialAllColumnsValid && partialAllGridsValid

fullSolutionWithDiags :: SudokuPuzzle -> Boolean
fullSolutionWithDiags = noEmptyCells && partialSolutionWithDiags

partialSolutionWithDiags :: SudokuPuzzle -> Boolean
partialSolutionWithDiags =
  partialSolutionNoDiags && partialTopLBottomR && partialTopRBottomL
