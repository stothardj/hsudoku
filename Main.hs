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

-- |Use the sudoku library to solve a sudoku board input in a formatted file.
--  The input file should only contain numbers separated by spacing. The first
--  number is the dimension of the board. For example this would be nine for a
--  standard sized board. The next number is the number of rows in one of the internal
--  boxes. This would be 3 in a standard board. The next number is the number of
--  columns in an internal box. This typically is also 3. These inputs are needed
--  since for a 6x6 board, for example, the solutions are different whether the internal
--  boxes are 2x3 or 3x2. To represent open squares, the one's that need to be filled in,
--  use the number 0 since this is not a valid number for any sized board. In an nxn
--  board the numbers 1 through n represent numbers which are known.
module Main (main) where

import Sudoku

-- |Chop a list into list-of-lists where each sublist is of length n
chopList :: [a] -> Int -> [[a]]
chopList [] n = []
chopList ls n =
  as : chopList bs n
  where
    (as, bs) = splitAt n ls

-- |Parse formatted input file of sudoku board and output all answers
main = do
  b <- getContents
  let nums = map read (words b) :: [Int]
  let n = nums !! 0
  let rn = nums !! 1
  let cn = nums !! 2
  let flatBoard = drop 3 nums
  let board = chopList flatBoard n
  mapM_ (putStrLn . showBoard) (solveBoard rn cn (initBoard board))
