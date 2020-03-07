# Sudoku Solver

I was challenged to write a program that can solve Sudoku puzzles in PureScript. This repo is my solution to that challenge. Tests are included.

## Current Approach: Brute Force

It utilizes a brute-force approach to solving the puzzle. An `Array` [Zipper](https://github.com/JordanMartinez/purescript-jordans-reference/blob/latestRelease/31-Design-Patterns/14-Zipper.md) was used to track which hole for which the solver is making a guess. The solver can be configured to check for any combination of the following:
- all rows are unique
- all columns are unique
- all grids are unique
- top-left to bottom-right diagonal is unique
- top-right to bottom-left diagonal is unique
- specific cells within the puzzle are unique

Moreover, the solver will work on any size of a Sudoku puzzle. Most puzzles utilize a 9x9 grid that contains 9 3x3 subgrids. This same code could work on a 16x16 grid that contains 16 4x4 subgrids.

## Drawbacks of Current Approach

The code does not yet allow a user to input a puzzle (e.g. via CLI args or a file) into the program, nor does the code validate such input as a valid Sudoku puzzle (i.e. it has holes, all numbers inputted are within the expected bounds, the initial puzzle is valid if holes are ignored).

The code does not pretty print the resulting puzzle back out in such a way that it looks like a Sudoku puzzle. However, the printed puzzle still indicates which number was the original number in the puzzle and which was a guess. Original numbers will have two spaces around them (e.g. ` 5 `) whereas guesses will have angled brackets around them (e.g. `<5>`). Any holes will be outputted as underscores with two spaces around them: ` _ `.

The current brute-force code could be made faster by modifying the code in the following ways...
- using a mutable `Matrix` to store the puzzle's data (current approach uses an immutable copy-on-write flat `Array` that feels like a Matrix via its API)
- using `ListZipper` rather than `ArrayZipper` for the Zipper used for hole-tracking, which is especially faster when resetting a hole's guesses (current approach uses the `ArrayZipper`, which will copy the entire array when updating a single hole in it)
- calculating the constraints that each hole must satisfy and only validating a single guess with those constraints (current approach unnecessarily validates the entire puzzle on each guess)
- using [`Aff`](https://pursuit.purescript.org/packages/purescript-aff/5.1.2/docs/Effect.Aff#t:Aff) to run 2+ concurrent fibers at the same time, stopping all fibers once any one of them finds a valid solution and killing the other two fibers

## Speeding up the Solver

Moreover, the code could be made faster by using an approach that is smarter than brute force. In such an approach, the solver would first try to identify any "obvious" guesses before defaulting back to a brute-force approach. Examples of "obvious" guesses using a normal 9x9-cell puzzle would include:
- a row/column/subgrid has 8 cells already filled in, so the the 9th cell must be 1 specific number
- a number appears 8 times throughout the puzzle, so the 9th appearance is known

In other words, the control flow might look like this:
1. If there is an obvious guess based on rows/columns/subgrids/appearances, make it and loop
2. Make a guess for the next possible hole.
3. If the guess is valid, loop. If it's not, backtrack and try the next guess.
4. If we backtrack to a hole that had an original "obvious" guess, the puzzle cannot be solved because it's invalid.
