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

module Test where

import Sudoku

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

