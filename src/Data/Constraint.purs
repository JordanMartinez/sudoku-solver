module Data.Constraint where

import Prelude

import Data.Array ((..))
import Data.Filterable (filterMap)
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..), reverse, (:))
import Data.Map (Map, insertWith)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.SudokuPuzzle (CellValue(..), SudokuPuzzle)
import Data.Tuple (Tuple(..))
import Matrix (get, getColumn, getRow, width)

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
