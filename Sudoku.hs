-- This file is part of hsudoku.
-- 
-- Hsudoku is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- Hsudoku is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
-- 
-- Copyright Jake Stothard 2012

-- |This library is used to solve any sized sudoku board. It aims
-- to be reasonably quick while also being easy to understand. The
-- solver finds all solutions at the same time and returns them in
-- a list. To understand usage look at Main.hs
module Sudoku
  (SudokuSquare,
   Grid,
   SudokuBoard,
   validBoardSize,
   initBoard,
   solveBoard,
   showBoard) where

import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Vector as V
import qualified Data.Set as S

-- |A sudoku square is a set of possible numbers which could go there
type SudokuSquare = S.Set Int
-- |Currently representing grid as a vector of vectors
type Grid a = V.Vector (V.Vector a)
-- |A sudoku board is a grid of sudoku squares
type SudokuBoard = Grid SudokuSquare

-- |Calculate fixed point. Apply f until x = f x
fixedPointM f x = do
  y <- f x
  if y == x
    then return x
    else fixedPointM f y

-- |Return whether a set only contains one element
isSingleton :: S.Set a -> Bool
isSingleton = (== 1) . S.size

-- |Concatenate a vector of vectors
concatVectors :: V.Vector (V.Vector a) -> V.Vector a
concatVectors = V.foldl (V.++) V.empty

-- |Slice a vector into num vectors of size size
multislice :: Int -> Int -> V.Vector a -> V.Vector (V.Vector a)
multislice size num g = V.generate num (\k -> V.slice (k*size) size g)

-- |Returns whether an nxn board with boxes of r rows and c cols is a valid sudoku board
validBoardSize :: Int   -- ^ The value n of an nxn board. That is, the total numbers across
               -> Int   -- ^ The number of rows in an internal box
               -> Int   -- ^ The number of cols in an internal box
               -> Bool
validBoardSize n r c = n == r * c

-- |Transform list-of-list structure into vector-of-vector structure
listsToGrid :: [[a]] -> Grid a
listsToGrid = V.fromList . map V.fromList

-- |Replace a number representation of a square with a set of possibilities from 1 to the size n.
--  The number zero is special, as it represents an open square
replaceOpenSquares :: Int -> Grid Int -> SudokuBoard
replaceOpenSquares n =
  V.map (V.map replace0)
  where
    replace0 x
      | x == 0 = S.fromList [1..n]
      | otherwise = S.singleton x

-- |Transform list-of-list board representation using zeroes as open squares
--  into internal representation
initBoard :: [[Int]] -> SudokuBoard
initBoard board = replaceOpenSquares n (listsToGrid board)
  where
    n = length board

-- |Given a vector of sudoku squares, use singletons which represent definite values
--  to remove from unsure possibilities.
--  Fails if there are two singletons with the same value or any empty sets
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

-- |Filter all rows using above function
filterRows :: SudokuBoard -> Maybe SudokuBoard
filterRows = V.mapM filterPossibilities

-- |Transpose vector-of-vectors. This may be expensive (maybe?) but would be cheap (built-in)
--  if anyone ever wanted to port this to using the repa library
transpose :: Grid a -> Grid a
transpose grid =
  V.generate n (`getCol` grid)
  where
    n = V.length grid
    getCol k = V.map (V.! k)

-- |Filter all cols
filterCols :: SudokuBoard -> Maybe SudokuBoard
filterCols = fmap transpose . filterRows . transpose

-- |Filter all boxes
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

-- |A single filter pass over the board which filters definites out of possibilities based on row, col, and box
filterPass :: Int -> Int -> SudokuBoard -> Maybe SudokuBoard
filterPass rn cn grid =
  filterRows grid >>= filterCols >>= filterBoxes rn cn

-- |Return whether the board is solved
boardSolved :: SudokuBoard -> Bool
boardSolved = V.all (V.all isSingleton)

-- |Replace a single sudoku board with two sudoku boards. The first is with a single
--  unsure spot replaced with a guess. The second is with that guess removed. Fails
--  if there are no unsure places in the board
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

-- |Solve a sudoku board. Returns a list of all possible solutions
solveBoard :: Int   -- ^ The number of rows in an internal board
           -> Int   -- ^ The number of cols in an internal board
           -> SudokuBoard -- ^ The sudoku board to solve
           -> [SudokuBoard]
solveBoard rn cn board = do
  filtered <- M.maybeToList $ fixedPointM (filterPass rn cn) board
  if boardSolved filtered
    then return filtered
    else do
           (board1, board2) <- M.maybeToList $ guess filtered
           solveBoard rn cn board1 `L.union` solveBoard rn cn board2

-- |Show a sudoku board in an easy to read format. Will look slightly
--  uglier if there are numbers greater than one digit long. Assumes
--  the board is solved, otherwise it will just print out one of the
--  possible numbers in that spot
showBoard :: SudokuBoard -> String
showBoard =
  L.unlines . V.toList . (V.map (unwords . V.toList . V.map (show . unset)))
  where
    unset :: SudokuSquare -> Int
    unset = head . S.elems
