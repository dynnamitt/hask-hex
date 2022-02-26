module Main where

import InfiniteHexGrid
import Data.Maybe
import FiniteHexGrid
import System.Random (getStdGen,mkStdGen)
import Data.List (zip, transpose, unfoldr)
import System.Environment
import System.Exit
import qualified System.Console.Terminal.Size as TS


main :: IO ()
main = do
  win <- TS.size
  drawGrid $ fromJust win


drawGrid :: TS.Window Int -> IO ()
drawGrid (TS.Window h w) = do
  let gen = mkStdGen $ wSeed world1
  let (x,y) = (div w 4, div h 4)
  let grid = initIHexGrid gen (0, wSize world1)
  let viewPort = ViewPort (w,h) (x,y) 2 world1
  mapM_ putStrLn $ finiteHexGrid viewPort grid
