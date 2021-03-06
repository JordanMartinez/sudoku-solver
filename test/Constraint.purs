module Test.Constraint where

import Prelude

import Data.Constraint (ColumnIndex(..), RowIndex(..), fullSolutionNoDiags, partialAllGridsValid, uniqueArray, uniqueColumn, uniqueDiagonalTopLBottomR, uniqueDiagonalTopRBottomL, uniqueGrid, uniqueIndices, uniqueRow)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromJust)
import Data.SudokuPuzzle (CellValue(..), SudokuPuzzle)
import Data.Tuple (Tuple(..))
import Matrix (fromArray)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it, pending)
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

  describe "uniqueGrid should work properly" do
    describe "no duplicates" do
      let
        p4x4Pass :: SudokuPuzzle
        p4x4Pass = mkPuzzle [ [Original 1, Original 2, Original 1, Original 4 ]
                            , [Guess    3, Guess    4, Guess    3, Guess    2 ]
                            , [Guess    4, Guess    3, Guess    4, Guess    3 ]
                            , [Guess    2, Guess    1, Guess    2, Guess    1 ]
                            ]
      it "top-left" do
        (uniqueGrid (Tuple (RowIndex 0) (ColumnIndex 0)) p4x4Pass) `shouldEqual` (Just Nil)
        (uniqueGrid (Tuple (RowIndex 0) (ColumnIndex 1)) p4x4Pass) `shouldEqual` (Just Nil)
        (uniqueGrid (Tuple (RowIndex 1) (ColumnIndex 0)) p4x4Pass) `shouldEqual` (Just Nil)
        (uniqueGrid (Tuple (RowIndex 1) (ColumnIndex 1)) p4x4Pass) `shouldEqual` (Just Nil)

      it "bottom-left" do
        (uniqueGrid (Tuple (RowIndex 2) (ColumnIndex 0)) p4x4Pass) `shouldEqual` (Just Nil)
        (uniqueGrid (Tuple (RowIndex 2) (ColumnIndex 1)) p4x4Pass) `shouldEqual` (Just Nil)
        (uniqueGrid (Tuple (RowIndex 3) (ColumnIndex 0)) p4x4Pass) `shouldEqual` (Just Nil)
        (uniqueGrid (Tuple (RowIndex 3) (ColumnIndex 1)) p4x4Pass) `shouldEqual` (Just Nil)

      it "top-right" do
        (uniqueGrid (Tuple (RowIndex 0) (ColumnIndex 2)) p4x4Pass) `shouldEqual` (Just Nil)
        (uniqueGrid (Tuple (RowIndex 0) (ColumnIndex 3)) p4x4Pass) `shouldEqual` (Just Nil)
        (uniqueGrid (Tuple (RowIndex 1) (ColumnIndex 2)) p4x4Pass) `shouldEqual` (Just Nil)
        (uniqueGrid (Tuple (RowIndex 1) (ColumnIndex 3)) p4x4Pass) `shouldEqual` (Just Nil)

      it "bottom-right" do
        (uniqueGrid (Tuple (RowIndex 2) (ColumnIndex 2)) p4x4Pass) `shouldEqual` (Just Nil)
        (uniqueGrid (Tuple (RowIndex 2) (ColumnIndex 3)) p4x4Pass) `shouldEqual` (Just Nil)
        (uniqueGrid (Tuple (RowIndex 3) (ColumnIndex 2)) p4x4Pass) `shouldEqual` (Just Nil)
        (uniqueGrid (Tuple (RowIndex 3) (ColumnIndex 3)) p4x4Pass) `shouldEqual` (Just Nil)

    describe "duplicates" do
      let
        p4x4Fail :: SudokuPuzzle
        p4x4Fail = mkPuzzle [ [Original 1, Original 2, Original 1, Original 4 ]
                            , [Guess    2, Guess    4, Guess    2, Guess    2 ]
                            , [Guess    4, Guess    2, Guess    3, Guess    2 ]
                            , [Guess    2, Guess    1, Guess    1, Guess    2 ]
                            ]
      it "top-left" do
        (uniqueGrid (Tuple (RowIndex 0) (ColumnIndex 0)) p4x4Fail) `shouldEqual` (Just ({number: 2, count: 2} : Nil))
        (uniqueGrid (Tuple (RowIndex 0) (ColumnIndex 1)) p4x4Fail) `shouldEqual` (Just ({number: 2, count: 2} : Nil))
        (uniqueGrid (Tuple (RowIndex 1) (ColumnIndex 0)) p4x4Fail) `shouldEqual` (Just ({number: 2, count: 2} : Nil))
        (uniqueGrid (Tuple (RowIndex 1) (ColumnIndex 1)) p4x4Fail) `shouldEqual` (Just ({number: 2, count: 2} : Nil))

      it "bottom-left" do
        (uniqueGrid (Tuple (RowIndex 2) (ColumnIndex 0)) p4x4Fail) `shouldEqual` (Just ({number: 2, count: 2} : Nil))
        (uniqueGrid (Tuple (RowIndex 2) (ColumnIndex 1)) p4x4Fail) `shouldEqual` (Just ({number: 2, count: 2} : Nil))
        (uniqueGrid (Tuple (RowIndex 3) (ColumnIndex 0)) p4x4Fail) `shouldEqual` (Just ({number: 2, count: 2} : Nil))
        (uniqueGrid (Tuple (RowIndex 3) (ColumnIndex 1)) p4x4Fail) `shouldEqual` (Just ({number: 2, count: 2} : Nil))

      it "top-right" do
        (uniqueGrid (Tuple (RowIndex 0) (ColumnIndex 2)) p4x4Fail) `shouldEqual` (Just ({number: 2, count: 2} : Nil))
        (uniqueGrid (Tuple (RowIndex 0) (ColumnIndex 3)) p4x4Fail) `shouldEqual` (Just ({number: 2, count: 2} : Nil))
        (uniqueGrid (Tuple (RowIndex 1) (ColumnIndex 2)) p4x4Fail) `shouldEqual` (Just ({number: 2, count: 2} : Nil))
        (uniqueGrid (Tuple (RowIndex 1) (ColumnIndex 3)) p4x4Fail) `shouldEqual` (Just ({number: 2, count: 2} : Nil))

      it "bottom-right" do
        (uniqueGrid (Tuple (RowIndex 2) (ColumnIndex 2)) p4x4Fail) `shouldEqual` (Just ({number: 2, count: 2} : Nil))
        (uniqueGrid (Tuple (RowIndex 2) (ColumnIndex 3)) p4x4Fail) `shouldEqual` (Just ({number: 2, count: 2} : Nil))
        (uniqueGrid (Tuple (RowIndex 3) (ColumnIndex 2)) p4x4Fail) `shouldEqual` (Just ({number: 2, count: 2} : Nil))
        (uniqueGrid (Tuple (RowIndex 3) (ColumnIndex 3)) p4x4Fail) `shouldEqual` (Just ({number: 2, count: 2} : Nil))

  describe "partialAllGridsValid should work properly" do
    let
      p4x4Pass :: SudokuPuzzle
      p4x4Pass = mkPuzzle [ [Original 1, Original 2, Original 1, Original 4 ]
                          , [Guess    3, Guess    4, Guess    3, Guess    2 ]
                          , [Guess    4, Guess    3, Guess    4, Guess    2 ]
                          , [Guess    2, Guess    1, Guess    3, Guess    1 ]
                          ]

      p4x4Fail :: SudokuPuzzle
      p4x4Fail = mkPuzzle [ [Original 1, Original 1, Original 1, Original 4 ]
                          , [Guess    3, Guess    4, Guess    2, Guess    2 ]
                          , [Guess    4, Guess    3, Guess    4, Guess    2 ]
                          , [Guess    1, Guess    1, Guess    2, Guess    1 ]
                          ]
    it "Expect pass" do
      (partialAllGridsValid p4x4Pass) `shouldEqual` true
    it "Expect failure" do
      (partialAllGridsValid p4x4Fail) `shouldEqual` false

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

  describe "fullSolutionNoDiags should work properly" do
    let
      p4x4Pass :: SudokuPuzzle
      p4x4Pass = mkPuzzle [ [Original 1, Original 2, Original 3, Original 4 ]
                          , [Guess    4, Guess    3, Guess    2, Guess    1 ]
                          , [Guess    3, Guess    1, Guess    4, Guess    2 ]
                          , [Guess    2, Guess    4, Guess    1, Guess    3 ]
                          ]

      p4x4Fail :: SudokuPuzzle
      p4x4Fail = mkPuzzle [ [Original 1, Original 2, Original 1, Original 4 ]
                          , [Guess    3, Guess    4, Guess    3, Guess    2 ]
                          , [Guess    4, Guess    3, Guess    4, Guess    2 ]
                          , [Guess    1, Guess    1, Guess    5, Guess    1 ]
                          ]

    it "a valid solution" do
      (fullSolutionNoDiags p4x4Pass) `shouldEqual` true
    it "an invalid solution" do
      (fullSolutionNoDiags p4x4Fail) `shouldEqual` false

  describe "fullSolutionWithDiags should work properly" do
    let
      p4x4Pass :: SudokuPuzzle
      p4x4Pass = mkPuzzle [ [Original 1, Original 2, Original 4, Original 3 ]
                          , [Guess    4, Guess    2, Guess    2, Guess    1 ]
                          , [Guess    3, Guess    1, Guess    3, Guess    2 ]
                          , [Guess    2, Guess    4, Guess    1, Guess    4 ]
                          ]

    pending "this test can only be run on a valid 9x9 puzzle. 2x2 isn't possible"
     -- do
     --  (fullSolutionWithDiags p4x4Pass) `shouldEqual` true

mkPuzzle :: Array (Array CellValue) -> SudokuPuzzle
mkPuzzle array = unsafePartial $ fromJust $ fromArray array
