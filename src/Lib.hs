module Lib
  ( getKey,
    emptyGrid,
    getEmptyCellCoords,
    addRandomTwo,
    collapseRowsToLeft,
    mergeCellsToLeft,
    noMovesPossible,
    r90c,
    r90cc,
    gridSize,
  )
where

import Data.List (transpose)
import System.IO (hReady, stdin)
import System.Random (StdGen, uniformR)

gridSize :: Int
gridSize = 4

getEmptyCellCoords :: [[Int]] -> [(Int, Int)]
getEmptyCellCoords grid = map (\(r, c, _) -> (r, c)) (filter (\(_, _, e) -> e == 0) grid')
  where
    grid' = concat [[(r, c, e) | (c, e) <- zip [0 ..] row] | (r, row) <- zip [0 ..] grid]

addRandomTwo :: StdGen -> [[Int]] -> [[Int]]
addRandomTwo gen grid = grid'
  where
    emptyCells = getEmptyCellCoords grid
    (r, c) = emptyCells !! fst (uniformR (0, length emptyCells - 1) gen :: (Int, StdGen))
    (rows, row : rowss) = splitAt r grid
    (cols, _ : colss) = splitAt c row
    newRow = cols ++ [2] ++ colss
    grid' = rows ++ [newRow] ++ rowss

emptyGrid :: [[Int]]
emptyGrid = replicate gridSize $ replicate gridSize 0

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
    getKey' chars = do
      char <- getChar
      more <- hReady stdin
      (if more then getKey' else return) (char : chars)

collapseRowsToLeft :: [[Int]] -> [[Int]]
collapseRowsToLeft = map (paddWithEmpty . filterEmpty)
  where
    filterEmpty = filter (/= 0)
    paddWithEmpty xs = xs ++ replicate (gridSize - length xs) 0

mergeCellsToLeft :: [[Int]] -> [[Int]]
mergeCellsToLeft = collapseRowsToLeft . map mergeRow . collapseRowsToLeft

r90c :: [[Int]] -> [[Int]]
r90c = transpose . reverse

r90cc :: [[Int]] -> [[Int]]
r90cc = reverse . transpose

mergeRow :: [Int] -> [Int]
mergeRow [] = []
mergeRow [n] = [n]
mergeRow (x1 : x2 : xs)
  | x1 == x2 = 2 * x1 : mergeRow xs
  | otherwise = x1 : mergeRow (x2 : xs)

noMovesPossible :: [[Int]] -> Bool
noMovesPossible grid = and [grid == ((f . mergeCellsToLeft . g) grid) | (f, g) <- fl]
  where
    fl = [(id, id), (r90cc, r90c), (r90c, r90cc), (r90c . r90c, r90cc . r90cc)]