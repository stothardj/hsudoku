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
