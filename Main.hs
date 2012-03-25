module Main where

import qualified Data.Vector as V
import qualified Data.Set as S

-- A sudoku square is a set of possible numbers which could go there
type SudokuSquare = S.Set Int
-- Currently representing grid as a vector of vectors
type Grid a = V.Vector (V.Vector a)
type SudokuBoard = Grid SudokuSquare

-- Calculate fixed point. Apply f until x = f x
-- TODO: Replace with Control.Monad.Fix
fixedPoint f x
  | y == x = x
  | otherwise = fixedPoint f y
  where
    y = f x

-- Returns whether an nxn board with boxes of r rows and c cols is a valid sudoku board
validBoardSize :: Int -> Int -> Int -> Bool
validBoardSize n r c = n == r * c

-- Transform list-of-list structure into vector-of-vector structure
listsToGrid :: [[a]] -> Grid a
listsToGrid = V.fromList . map V.fromList

-- Replace a number representation of a square with a set of possibilities from 1 to the size n
-- The number zero is special, as it represents an open square
replaceOpenSquares :: Int -> Grid Int -> SudokuBoard
replaceOpenSquares n =
  V.map (V.map replace0)
  where
    replace0 x
      | x == 0 = S.fromList [1..n]
      | otherwise = S.singleton x

-- Transform list-of-list board representation using zeroes as open squares
-- into internal representation
initBoard :: [[Int]] -> SudokuBoard
initBoard board = replaceOpenSquares n (listsToGrid board)
  where
    n = length board

-- Given a vector of sudoku squares, use singletons which represent definite values
-- to remove from unsure possibilities
filterPossibilities :: V.Vector SudokuSquare -> V.Vector SudokuSquare
filterPossibilities ps =
  V.map removeImpossible ps
  where
    isSingleton = (== 1) . S.size
    accumulateSingletons acc x
      | isSingleton x = x `S.union` acc
      | otherwise = acc
    definites = V.foldl accumulateSingletons S.empty ps
    removeImpossible p
      | isSingleton p = p
      | otherwise = S.difference p definites

-- Filter all rows using above function
filterRows :: SudokuBoard -> SudokuBoard
filterRows = V.map filterPossibilities

-- Transpose vector-of-vectors. This may be expensive (maybe?) but would be cheap (built-in)
-- if anyone ever wanted to port this to using the repa library
transpose :: Grid a -> Grid a
transpose grid =
  V.generate n (`getCol` grid)
  where
    n = V.length grid
    getCol k = V.map (V.! k)

-- Filter all cols
filterCols :: SudokuBoard -> SudokuBoard
filterCols = transpose . filterRows . transpose

-- Concatenate a vector of vectors
concatVectors = V.foldl (V.++) V.empty

-- Slice a vector into num vectors of size size
multislice size num g = V.generate num (\k -> V.slice (k*size) size g)

-- Filter all boxes
filterBoxes rn cn grid =
  (intoBoxes . filterRows . intoBoxes) grid
  where
    numRows = V.length grid
    numCols = V.length (V.head grid)
    numBoxH = numCols `div` cn
    numBoxV = numRows `div` rn
    pos = [br*numCols*rn+r*numCols+bc*cn+c | br <- [0..numBoxV-1], bc <- [0..numBoxH-1], r <- [0..rn-1], c <- [0..cn-1]]
    intoBoxes = multislice (rn*cn) (numBoxH*numBoxV) . (`V.backpermute` V.fromList pos) . concatVectors

-- -- A single filter pass over the board which filters definites out of possibilities based on row, col, and box
-- filterPass n rn cn board =
--   filterBoxes n rn cn board''
--   where
--     board' = filterRows n n board
--     board'' = filterCols n n board'

-- Test Cases

-- Board 1
board1 = initBoard [
  [0, 0, 5, 0, 1, 2, 0, 4, 7],
  [8, 3, 0, 6, 0, 0, 0, 0, 0],
  [0, 2, 1, 9, 0, 4, 3, 6, 5],
  [3, 1, 0, 0, 0, 6, 5, 0, 2],
  [0, 5, 8, 1, 0, 0, 0, 0, 0],
  [4, 0, 6, 5, 2, 8, 0, 9, 0],
  [1, 0, 2, 4, 9, 0, 7, 5, 0],
  [5, 8, 0, 0, 0, 0, 4, 0, 0],
  [0, 4, 0, 8, 7, 0, 2, 1, 6]
  ]



