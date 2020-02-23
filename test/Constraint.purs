module Test.Constraint where

import Prelude

import Data.Constraint (ColumnIndex(..), RowIndex(..), uniqueArray, uniqueColumn, uniqueDiagonalTopLBottomR, uniqueDiagonalTopRBottomL, uniqueIndices, uniqueRow)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromJust)
import Data.SudokuPuzzle (CellValue(..), SudokuPuzzle)
import Data.Tuple (Tuple(..))
import Matrix (fromArray)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Constraints" do
  describe "uniqueArray should work properly" do
    it "unique values have no duplicates" do
      (uniqueArray [1, 2, 3, 4]) `shouldEqual` Nil
    it "non-unique values have 1 or more duplicates" do
      (uniqueArray [1, 1, 3, 4]) `shouldEqual` ({number: 1, count: 2} : Nil)
      (uniqueArray [1, 1, 1, 4]) `shouldEqual` ({number: 1, count: 3} : Nil)
      (uniqueArray [1, 1, 2, 2]) `shouldEqual` ({number: 1, count: 2} : {number: 2, count: 2} : Nil)

  describe "uniqueRow should work properly" do
    let
      p2x2Pass :: SudokuPuzzle
      p2x2Pass = mkPuzzle [ [Original 1, Guess 2]
                          , [Empty     , Empty  ]
                          ]

      p2x2Fail :: SudokuPuzzle
      p2x2Fail = mkPuzzle [ [Original 1, Guess 1]
                          , [Empty     , Empty  ]
                          ]

    it "a row with no duplicates" do
      (uniqueRow (RowIndex 0) p2x2Pass) `shouldEqual` (Just Nil)
    it "a row with 1 or more duplicates" do
      (uniqueRow (RowIndex 0) p2x2Fail) `shouldEqual` (Just ({number: 1, count: 2} : Nil))

  describe "uniqueColumn should work properly" do
    let
      p2x2Pass :: SudokuPuzzle
      p2x2Pass = mkPuzzle [ [Original 1, Empty ]
                          , [Guess    2, Empty ]
                          ]

      p2x2Fail :: SudokuPuzzle
      p2x2Fail = mkPuzzle [ [Original 1, Empty ]
                          , [Guess    1, Empty ]
                          ]
    it "a column with no duplicates" do
      (uniqueColumn (ColumnIndex 0) p2x2Pass) `shouldEqual` (Just Nil)
    it "a column with one or more duplicates" do
      (uniqueColumn (ColumnIndex 0) p2x2Fail) `shouldEqual` (Just ({number: 1, count: 2} : Nil))

  describe "uniqueIndices should work properly" do
    let
      p2x2Pass :: SudokuPuzzle
      p2x2Pass = mkPuzzle [ [Original 1, Empty   ]
                          , [Guess    2, Guess 3 ]
                          ]

      p2x2Fail :: SudokuPuzzle
      p2x2Fail = mkPuzzle [ [Original 1, Empty   ]
                          , [Guess    2, Guess 1 ]
                          ]

      row0 = RowIndex 0
      row1 = RowIndex 1
      col0 = ColumnIndex 0
      col1 = ColumnIndex 1
      t00_01_11 = [Tuple row0 col0, Tuple row0 col1, Tuple row1 col1]

    it "a section with no duplicates" do
      (uniqueIndices t00_01_11 p2x2Pass) `shouldEqual` (Just Nil)
    it "non-unique values in a row should cause a failure" do
      (uniqueIndices t00_01_11 p2x2Fail) `shouldEqual` (Just ({number: 1, count: 2} : Nil))

  describe "uniqueDiagonalTopLBottomR should work properly" do
    let
      p2x2Pass :: SudokuPuzzle
      p2x2Pass = mkPuzzle [ [Original 1, Empty   ]
                          , [Empty     , Guess 2 ]
                          ]

      p2x2Fail :: SudokuPuzzle
      p2x2Fail = mkPuzzle [ [Original 1, Empty   ]
                          , [Empty     , Guess 1 ]
                          ]

    it "a top-left to bottom-right diagonal with no duplicates" do
      (uniqueDiagonalTopLBottomR p2x2Pass) `shouldEqual` (Just Nil)
    it "a top-left to bottom-right diagonal with 1 or more duplicates" do
      (uniqueDiagonalTopLBottomR p2x2Fail) `shouldEqual` (Just ({number: 1, count: 2} : Nil))

  describe "uniqueDiagonalTopRBottomL should work properly" do
    let
      p2x2Pass :: SudokuPuzzle
      p2x2Pass = mkPuzzle [ [Empty  , Original 1 ]
                          , [Guess 2, Empty      ]
                          ]

      p2x2Fail :: SudokuPuzzle
      p2x2Fail = mkPuzzle [ [Empty  , Original 1 ]
                          , [Guess 1, Empty      ]
                          ]

    it "a top-left to bottom-right diagonal with no duplicates" do
      (uniqueDiagonalTopRBottomL p2x2Pass) `shouldEqual` (Just Nil)
    it "a top-left to bottom-right diagonal with 1 or more duplicates" do
      (uniqueDiagonalTopRBottomL p2x2Fail) `shouldEqual` (Just ({number: 1, count: 2} : Nil))


mkPuzzle :: Array (Array CellValue) -> SudokuPuzzle
mkPuzzle array = unsafePartial $ fromJust $ fromArray array
