module Data.Constraint where

import Prelude

import Data.Array ((..))
import Data.Filterable (filterMap)
import Data.Foldable (foldl, all)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (floor, quot, rem, toNumber)
import Data.List (List(..), null, reverse, (:))
import Data.Map (Map, insertWith)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.SudokuPuzzle (CellValue(..), SudokuPuzzle, isEmptyCell, puzzle2x2)
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
    gridLength = floor (sqrt (toNumber (width puzzle))) -- square root
    rowStart = (row `quot` gridLength) * gridLength
    rowEnd = rowStart + gridLength - 1
    colStart = (col `quot` gridLength) * gridLength
    colEnd = colStart + gridLength - 1

{-
Given b => Add to array | Loop With
0, 0    => 0, 0         | 0, 1
0, 1    => 0, 1         | 0, 2
0, 2    => 1, 0         | 1, 1
1, 1    => 1, 1         | 1, 2
1, 2    => Nothing
-}
    indices :: Array (Tuple RowIndex ColumnIndex)
    indices =
      (Tuple rowStart colStart) # unfoldr (\(Tuple rowIdx colIdx) ->
        if colIdx <= colEnd
        then Just (Tuple (Tuple (RowIndex rowIdx) (ColumnIndex colIdx)) (Tuple rowIdx (colIdx + 1)))
        else if rowIdx < rowEnd
          then Just (Tuple (Tuple (RowIndex (rowIdx + 1)) (ColumnIndex colStart)) (Tuple (rowIdx + 1) (colStart + 1)))
          else Nothing
      )
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

noEmptyCells :: Array CellValue -> Boolean
noEmptyCells = all (not <<< isEmptyCell)

validSolutionNoDiags :: SudokuPuzzle -> Boolean
validSolutionNoDiags puzzle =
  (validPartialSolutionNoDiags puzzle) && (all (not <<< isEmptyCell) puzzle)

validPartialSolutionNoDiags :: SudokuPuzzle -> Boolean
validPartialSolutionNoDiags puzzle =
  allRowsValid
  && allColumnsValid
  where
    noDuplicatesFound array = null (uniqueArray (filterMap extractAndKeepInts array))
    allRowsValid = all noDuplicatesFound (rows puzzle)
    allColumnsValid = all noDuplicatesFound (columns puzzle)

validSolutionWithDiags :: SudokuPuzzle -> Boolean
validSolutionWithDiags puzzle =
  validPartialSolutionWithDiags puzzle && (all (not <<< isEmptyCell) puzzle)

validPartialSolutionWithDiags :: SudokuPuzzle -> Boolean
validPartialSolutionWithDiags puzzle =
  allRowsValid
  && allColumnsValid
  && validDiagonalTopLeftBottomRight
  && validDiagonalTopRightBottomLeft
  where
    noDuplicatesFound array = null (uniqueArray (filterMap extractAndKeepInts array))
    allRowsValid = all noDuplicatesFound (rows puzzle)
    allColumnsValid = all noDuplicatesFound (columns puzzle)
    validDiagonalTopLeftBottomRight = uniqueDiagonalTopLBottomR puzzle == Just Nil
    validDiagonalTopRightBottomLeft = uniqueDiagonalTopRBottomL puzzle == Just Nil
