module Main where

import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Vector as V
import qualified Data.Set as S

-- A sudoku square is a set of possible numbers which could go there
type SudokuSquare = S.Set Int
-- Currently representing grid as a vector of vectors
type Grid a = V.Vector (V.Vector a)
type SudokuBoard = Grid SudokuSquare

-- Calculate fixed point. Apply f until x = f x
fixedPointM f x = do
  y <- f x
  if y == x
    then return x
    else fixedPointM f y

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
      | isSingleton p = Just p
      | S.null diff = Nothing
      | otherwise = Just diff
      where diff = p S.\\ defs

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

-- Return whether the board is solved
boardSolved :: SudokuBoard -> Bool
boardSolved = V.all (V.all isSingleton)

-- Replace a single sudoku board with two sudoku boards. The first is with a single
-- unsure spot replaced with a guess. The second is with that guess removed. Fails
-- if there are no unsure places in the board
guess :: SudokuBoard -> Maybe (SudokuBoard, SudokuBoard)
guess board = do
  unsureRow <- V.findIndex M.isJust firstUnsureInRow
  unsureCol <- firstUnsureInRow V.! unsureRow
  let (min, others) = S.deleteFindMin (board V.! unsureRow V.! unsureCol)
  let tr = board V.! unsureRow
  let row1 = V.update tr (V.singleton (unsureCol, S.singleton min))
  let row2 = V.update tr (V.singleton (unsureCol, others))
  let board1 = V.update board (V.singleton (unsureRow, row1))
  let board2 = V.update board (V.singleton (unsureRow, row2))
  return (board1, board2)
  where
    unsure = (> 1) . S.size
    firstUnsureInRow = V.map (V.findIndex unsure) board

-- Solve a sudoku board
solveBoard :: Int -> Int -> SudokuBoard -> [SudokuBoard]
solveBoard rn cn board = do
  filtered <- M.maybeToList $ fixedPointM (filterPass rn cn) board
  if boardSolved filtered
    then return filtered
    else do
           (board1, board2) <- M.maybeToList $ guess filtered
           (solveBoard rn cn board1) `L.union` (solveBoard rn cn board2)

-- Test Cases
-- Board 1. Inner boxes are 3x3
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

-- Board 2. Inner boxes are 3x3
board2 = initBoard [
  [8, 6, 0, 3, 5, 4, 2, 1, 0],
  [0, 1, 0, 2, 8, 0, 5, 6, 3],
  [0, 0, 2, 1, 6, 0, 4, 0, 9],
  [0, 0, 7, 0, 0, 0, 1, 9, 0],
  [9, 3, 0, 0, 0, 1, 0, 2, 4],
  [0, 0, 0, 4, 0, 6, 0, 0, 5],
  [5, 2, 3, 0, 1, 8, 9, 0, 6],
  [0, 0, 8, 0, 3, 0, 0, 0, 1],
  [0, 0, 6, 9, 4, 5, 0, 3, 0]
  ]

-- Board 3. Inner boxes are 2x3
board3 = initBoard [
  [0, 0, 0, 0, 2, 0],
  [1, 0, 0, 3, 0, 0],
  [0, 2, 0, 5, 0, 0],
  [0, 0, 1, 0, 0, 0],
  [0, 0, 0, 0, 0, 5],
  [5, 4, 0, 6, 0, 0]
  ]

-- Board 4. Inner boxes are 2x2
board4 = initBoard [
  [1, 0, 0, 0],
  [0, 0, 0, 0],
  [0, 0, 0, 0],
  [0, 0, 0, 0]
  ]

