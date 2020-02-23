module Data.Constraint where

import Prelude

import Control.Alt (alt)
import Data.Array (all, fromFoldable, (..))
import Data.Filterable (filterMap)
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..), null, reverse, (:))
import Data.Map (Map, insertWith, values)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.SudokuPuzzle (CellValue(..), SudokuPuzzle, isEmptyCell)
import Data.Tuple (Tuple(..))
import Matrix (get, getColumn, getRow, toIndexedArray, width)

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
  row <- getColumn idx puzzle
  let realValues = filterMap extractAndKeepInts row
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

uniqueDiagonalTopLBottomR :: SudokuPuzzle -> Maybe (List Duplicate)
uniqueDiagonalTopLBottomR puzzle =
  uniqueIndices indexArray puzzle
  where
    indexArray = (0 .. (width puzzle)) <#> (\i -> Tuple (RowIndex i) (ColumnIndex i))

uniqueDiagonalTopRBottomL :: SudokuPuzzle -> Maybe (List Duplicate)
uniqueDiagonalTopRBottomL puzzle =
  uniqueIndices indexArray puzzle
  where
    size = width puzzle
    indexArray = (0 .. size) <#> (\i -> Tuple (RowIndex i) (ColumnIndex (size - i - 1)))

noEmptyCells :: Array CellValue -> Boolean
noEmptyCells = all (not <<< isEmptyCell)

validSolutionNoDiags :: SudokuPuzzle -> Boolean
validSolutionNoDiags puzzle =
  allCellsFilled
  && allRowsValid
  && allColumnsValid
  where
    indexedArray = toIndexedArray puzzle
    allCellsFilled = all (not <<< isEmptyCell <<< _.value) indexedArray
    noDuplicatesFound array = null (uniqueArray (filterMap extractAndKeepInts array))
    allRowsValid = all noDuplicatesFound (rows indexedArray)
    allColumnsValid = all noDuplicatesFound (columns indexedArray)

validSolutionWithDiags :: SudokuPuzzle -> Boolean
validSolutionWithDiags puzzle =
  allCellsFilled
  && allRowsValid
  && allColumnsValid
  && validDiagonalTopLeftBottomRight
  && validDiagonalTopRightBottomLeft
  where
    indexedArray = toIndexedArray puzzle
    allCellsFilled = all (not <<< isEmptyCell <<< _.value) indexedArray
    noDuplicatesFound array = null (uniqueArray (filterMap extractAndKeepInts array))
    allRowsValid = all noDuplicatesFound (rows indexedArray)
    allColumnsValid = all noDuplicatesFound (columns indexedArray)
    validDiagonalTopLeftBottomRight = uniqueDiagonalTopLBottomR puzzle == Just Nil
    validDiagonalTopRightBottomLeft = uniqueDiagonalTopRBottomL puzzle == Just Nil

rows :: forall a. Array { x :: Int, y :: Int, value :: a } -> Array (Array a)
rows indexedArray =
  let
    groupByY :: Map Int (Array a) -> { x :: Int, y :: Int, value :: a } -> Map Int (Array a)
    groupByY groupMap nextRec =
      insertWith (\old next -> old `alt` next) nextRec.y [nextRec.value] groupMap

  in
    fromFoldable $ values $ foldl groupByY M.empty indexedArray

columns :: forall a. Array { x :: Int, y :: Int, value :: a } -> Array (Array a)
columns indexedArray =
  let
    groupByX :: Map Int (Array a) -> { x :: Int, y :: Int, value :: a } -> Map Int (Array a)
    groupByX groupMap nextRec =
      insertWith (\old next -> old `alt` next) nextRec.x [nextRec.value] groupMap

  in
    fromFoldable $ values $ foldl groupByX M.empty indexedArray
