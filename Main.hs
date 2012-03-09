module Main where

import qualified Data.Map

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

-- Normalize possibility to sensical representation
normalizePossibility (Possibly []) = Impossible
normalizePossibility (Possibly [x]) = Definitely x
normalizePossibility x = x

-- Transform list-of-list structure into map of coord to data
boxToMap box =
  Data.Map.fromList $ concatMap (map reorder) ((addColNums . addRowNums) box)
  where
    addColNums = map $ zip [0..]
    addRowNums = zipWith (\n r -> zip (repeat n) r) [0..]
    reorder (col,(row,item)) = ((row,col),item) 

-- Filter out all definite squares from other possible squares in given coords
filterCoords board coords =
  Data.Map.unions [remainingPossibilities, notInRange, definites]
  where
    ( inRange, notInRange ) = Data.Map.partitionWithKey (\k v -> k `elem` coords) board
    isDefinite (Definitely _) = True
    isDefinite (Possibly _) = False
    ( definites, possibles ) = Data.Map.partition isDefinite inRange
    removePossibilities defs (Possibly ls) =
      normalizePossibility $ Possibly (filter (\x -> Definitely x `notElem` defs) ls)
    remainingPossibilities =
      Data.Map.map (removePossibilities $ Data.Map.elems definites) possibles

-- Filter out all definite squares from row r of length l
filterRow board r l = filterCoords board [(r,i) | i <- [0..l-1]]

-- Filter out all definite squares from col c of length l
filterCol board c l = filterCoords board [(i,c) | i <- [0..l-1]]

-- Filter n rows/cols
filterN f board n l = foldl (\ accum x -> f accum x l) board [0..n-1]
filterRows = filterN filterRow
filterCols = filterN filterCol

main = do
  putStrLn "Hello"

