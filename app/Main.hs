module Main (main) where

import Lib
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
import System.Random

paddingLength :: Int
paddingLength = 4

whiteBlock :: Int -> String
whiteBlock n = "\ESC[107m" ++ replicate (n * paddingLength) ' ' ++ "\ESC[0m"

wrapInWhiteBlocks :: String -> String
wrapInWhiteBlocks s = whiteBlock 1 ++ s ++ whiteBlock 1

colorMap :: Int -> String
colorMap n = "\ESC[" ++ show (round (100.0 + logBase 2 (fromIntegral n))) ++ "m"

pprWrapped :: Int -> String
pprWrapped 0 = ppr 0
pprWrapped n = colorMap n ++ ppr n ++ "\ESC[0m"

ppr :: Int -> String
ppr n
  | n == 0 = replicate paddingLength ' '
  | otherwise = dn ++ replicate (paddingLength - length dn) ' '
  where
    dn = show n

aux :: [[Int]] -> IO Int
aux grid = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  putStrLn "\ESC[2J"
  print (sum (map sum grid))
  putStrLn (whiteBlock (gridSize + 2))
  mapM_ (putStrLn . wrapInWhiteBlocks . concatMap pprWrapped) grid
  putStrLn (whiteBlock (gridSize + 2))

  if noMovesPossible grid
    then return (sum (map sum grid))
    else do
      key <- getKey
      let (_, p, f) = case key of
            "\ESC[A" -> ("↑", r90cc, r90c)
            "\ESC[B" -> ("↓", r90c, r90cc)
            "\ESC[C" -> ("→", r90c . r90c, r90cc . r90cc)
            "\ESC[D" -> ("←", id, id)
            _ -> ("helzp", id, id)

      g <- newStdGen

      let newGrid = f (mergeCellsToLeft (p grid))
      if grid == newGrid
        then
          aux newGrid
        else
          aux (addRandomTwo g newGrid)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  g <- newStdGen
  score <- aux (addRandomTwo g emptyGrid)
  print ("you lost, score: " ++ show score)
