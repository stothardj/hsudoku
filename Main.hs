module Main where

import qualified Data.Map

-- Use "length" which returns Int. Unlikely able to solve boards larger than Int size
data SudokuSquare = Definitely Int | Possibly [Int] | Impossible
  deriving (Show,Eq)

type SudokuRow = [SudokuSquare]
type SudokuBox = [SudokuRow]

-- Calculate fixed point. Apply f until x = f x
fixedPoint f x
  | y == x = x
  | otherwise = fixedPoint f y
  where
    y = f x

-- Returns whether an nxn board with boxes of r rows and c cols is a valid sudoku board
validBoardSize :: Int -> Int -> Int -> Bool
validBoardSize n r c = n == r * c

-- Normalize possibility to sensical representation
normalizePossibility (Possibly []) = Impossible
normalizePossibility (Possibly [x]) = Definitely x
normalizePossibility x = x

-- Transform list-of-list numbers into sudoku squares. A zero represents an empty box
parseNumBoard b = 
  map (map replace0) b
  where
    n = length b
    replace0 x
      | x == 0 = Possibly [1..n]
      | otherwise = Definitely x

-- Transform list-of-list structure into map of coord to data
boxToMap box =
  Data.Map.fromList $ concatMap (map reorder) ((addColNums . addRowNums) box)
  where
    addColNums = map $ zip [0..]
    addRowNums = zipWith (\n r -> zip (repeat n) r) [0..]
    reorder (col,(row,item)) = ((row,col),item) 

-- Transform input representation into internal representation
initBoard = boxToMap . parseNumBoard

-- Filter out all definite squares from other possible squares in given coords
filterCoords board coords =
  Data.Map.unions [remainingPossibilities, notInRange, definites]
  where
    ( inRange, notInRange ) = Data.Map.partitionWithKey (\k v -> k `elem` coords) board
    isDefinite (Definitely _) = True
    isDefinite _ = False
    ( definites, possibles ) = Data.Map.partition isDefinite inRange
    removePossibilities defs (Possibly ls) =
      normalizePossibility $ Possibly (filter (\x -> Definitely x `notElem` defs) ls)
    removePossibilities defs Impossible = Impossible
    remainingPossibilities =
      Data.Map.map (removePossibilities $ Data.Map.elems definites) possibles

-- Filter out all definite squares from row r of length l
filterRow board r l = filterCoords board [(r,i) | i <- [0..l-1]]

-- Filter out all definite squares from col c of length l
filterCol board c l = filterCoords board [(i,c) | i <- [0..l-1]]

-- Filter out all definite squares from box top-left corner at (r,c) of rn rows, cn cols
filterBox board r rn c cn = filterCoords board [(i,j) | i <- [r..r+rn-1], j <- [c..c+cn-1]]

-- Filter n rows/cols
filterN f board n l = foldl (\ accum x -> f accum x l) board [0..n-1]
filterRows = filterN filterRow
filterCols = filterN filterCol

-- Filter boxes each with rn width and cn height
filterBoxes board n rn cn = 
  foldl foldOverCol board [0,cn..n-1] 
  where
    foldOverCol cboard c = foldl (\ accum r -> filterBox accum r rn c cn) cboard [0,rn..n-1]

main = do
  putStrLn "Hello"


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



