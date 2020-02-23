module Data.SudokuPuzzle where

import Prelude

import Data.Maybe (fromJust)
import Data.Monoid (power)
import Matrix (Matrix, fromArray, prettyPrintMatrix, width)
import Partial.Unsafe (unsafePartial)

-- | An individual cell in a Sudoku Puzzle:
-- | - `Original` represents the original puzzle's filled-in cell.
-- | - `Empty` represents the cells we have yet to make a guess in.
-- | - `Guess` represents cells we have made a guess in.
-- |
-- | Isomorphic to the following hypothetical data type:
-- | - Either OriginalInt (Maybe GuessInt)
data CellValue
  = Original Int
  | Empty
  | Guess Int

isOriginalCell :: CellValue -> Boolean
isOriginalCell = case _ of
  Original _ -> true
  _ -> false

isEmptyCell :: CellValue -> Boolean
isEmptyCell = case _ of
  Empty -> true
  _ -> false

isGuessCell :: CellValue -> Boolean
isGuessCell = case _ of
  Guess _ -> true
  _ -> false

instance showCellValue :: Show CellValue where
  show = case _ of
    Original i -> " " <> show i <> " "
    Empty -> "   "
    Guess i -> "<" <> show i <> ">"

type SudokuPuzzle = Matrix CellValue

puzzle2x2 :: SudokuPuzzle
puzzle2x2 =
  unsafePartial $ fromJust $ fromArray
  --            0           1
    [ [Original 1, Empty     ]  -- 0
    , [Empty     , Original 1]  -- 1
    ]

puzzle9x9 :: SudokuPuzzle
puzzle9x9 =
  unsafePartial $ fromJust $ fromArray
  --             0            1           2           3           4           5           6           7           8
    [ [ Original 2, Empty     , Empty     , Empty     , Original 4, Empty     , Empty     , Empty     , Empty      ] -- 0
    , [ Empty     , Original 5, Original 4, Empty     , Empty     , Empty     , Original 1, Empty     , Empty      ] -- 1
    , [ Original 6, Original 8, Empty     , Original 2, Empty     , Empty     , Original 4, Empty     , Empty      ] -- 2
    , [ Original 8, Empty     , Empty     , Original 1, Empty     , Original 9, Original 6, Original 4, Empty      ] -- 3
    , [ Original 3, Original 4, Empty     , Original 6, Original 2, Original 7, Empty     , Original 5, Original 9 ] -- 4
    , [ Empty     , Original 7, Original 6, Original 8, Empty     , Original 4, Empty     , Empty     , Original 3 ] -- 5
    , [ Empty     , Empty     , Original 2, Empty     , Empty     , Original 6, Empty     , Original 3, Original 4 ] -- 6
    , [ Empty     , Empty     , Original 7, Empty     , Empty     , Empty     , Original 5, Original 6, Empty      ] -- 7
    , [ Empty     , Empty     , Empty     , Empty     , Original 7, Empty     , Empty     , Empty     , Original 1 ] -- 8
    ]

printPuzzle :: SudokuPuzzle -> String
printPuzzle puzzle =
  power "=" (3 * width puzzle) <> "\n" <>
  prettyPrintMatrix show puzzle <> "\n" <>
  power "=" (3 * width puzzle)
