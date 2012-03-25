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

-- Return whether a set only contains one element
isSingleton :: S.Set a -> Bool
isSingleton = (== 1) . S.size

-- Concatenate a vector of vectors
concatVectors :: V.Vector (V.Vector a) -> V.Vector a
concatVectors = V.foldl (V.++) V.empty

-- Slice a vector into num vectors of size size
multislice :: Int -> Int -> V.Vector a -> V.Vector (V.Vector a)
multislice size num g = V.generate num (\k -> V.slice (k*size) size g)

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
-- Fails if there are two singletons with the same value or any empty sets
filterPossibilities :: V.Vector SudokuSquare -> Maybe (V.Vector SudokuSquare)
filterPossibilities ps = do
  definites <- V.foldM accumulateSingletons S.empty ps
  V.mapM (removeImpossible definites) ps
  where
    accumulateSingletons acc x
      | not $ isSingleton x = Just acc
      | x `S.isSubsetOf` acc = Nothing
      | otherwise = Just (x `S.union` acc)
    removeImpossible defs p
      | S.null p = Nothing
      | isSingleton p = Just p
      | otherwise = Just (S.difference p defs)

-- Filter all rows using above function
filterRows :: SudokuBoard -> Maybe SudokuBoard
filterRows = V.mapM filterPossibilities

-- Transpose vector-of-vectors. This may be expensive (maybe?) but would be cheap (built-in)
-- if anyone ever wanted to port this to using the repa library
transpose :: Grid a -> Grid a
transpose grid =
  V.generate n (`getCol` grid)
  where
    n = V.length grid
    getCol k = V.map (V.! k)

-- Filter all cols
filterCols :: SudokuBoard -> Maybe SudokuBoard
filterCols = fmap transpose . filterRows . transpose

-- Filter all boxes
filterBoxes :: Int -> Int -> SudokuBoard -> Maybe SudokuBoard
filterBoxes rn cn grid =
  (fmap intoBoxes . filterRows . intoBoxes) grid
  where
    numRows = V.length grid
    numCols = V.length (V.head grid)
    numBoxH = numCols `div` cn
    numBoxV = numRows `div` rn
    pos = [br*numCols*rn+r*numCols+bc*cn+c | br <- [0..numBoxV-1], bc <- [0..numBoxH-1], r <- [0..rn-1], c <- [0..cn-1]]
    intoBoxes = multislice (rn*cn) (numBoxH*numBoxV) . (`V.backpermute` V.fromList pos) . concatVectors

-- A single filter pass over the board which filters definites out of possibilities based on row, col, and box
filterPass :: Int -> Int -> SudokuBoard -> Maybe SudokuBoard
filterPass rn cn grid =
  filterRows grid >>= filterCols >>= filterBoxes rn cn

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



