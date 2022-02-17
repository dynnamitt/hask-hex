module Main where

import Colors
import InfiniteHexGrid
import System.Random (getStdGen)
import Data.List (zip, transpose, unfoldr)

seed = 2022
screenLen = 20

biomes = "-|*¤X" -- PutInto data w rangeInput
rRange = (0, length biomes - 1)

main :: IO ()
main = drawGrid screenLen screenLen


drawGrid :: Int -> Int -> IO ()
drawGrid maxCols maxRows = do
  g <- getStdGen
  let (x,y) = (10,10)
  let grid = initIHexGrid g rRange
  let fGrid = finiteHexGrid (maxCols,maxRows) (x,y) grid
  let rasterized = map (zoomRow2x biomes) fGrid
  let nicerRows = map (\r -> bgC 0 ++ r ++ toNorm) rasterized
  mapM_ putStrLn nicerRows

zoomRow2x :: [Char] -> FiniteRow -> String
zoomRow2x biomeSet (off, x:xs) =
    maybeCap off x ++ concatMap complete (init xs) ++ maybeCap off (last xs)
  where
    maybeCap CappedEnds = capped
    maybeCap Complete = complete
    capped = (:[]) . (!!) biomeSet
    complete = replicate 2 . (!!) biomeSet
