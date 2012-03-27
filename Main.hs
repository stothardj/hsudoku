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

module Main where

import Sudoku

-- Chop a list into list-of-lists where each sublist is of length n
chopList :: [a] -> Int -> [[a]]
chopList [] n = []
chopList ls n =
  as : chopList bs n
  where
    (as, bs) = splitAt n ls

-- Parse formatted input file of sudoku board and output all answers
main = do
  b <- getContents
  let nums = map read (words b) :: [Int]
  let n = nums !! 0
  let rn = nums !! 1
  let cn = nums !! 2
  let flatBoard = drop 3 nums
  let board = chopList flatBoard n
  print $ solveBoard rn cn (initBoard board)
