module Main where

import InfiniteHexGrid
import Data.Maybe
import FiniteHexGrid
import Worlds
import System.Random (getStdGen,mkStdGen)
import Data.List (zip, transpose, unfoldr)
import System.Environment
import qualified System.Console.Terminal.Size as TS

main :: IO ()
main = do
  wrld <- parseArgs 1
  win <- TS.size
  drawGrid ( fromJust win ) wrld

-- args or death
parseArgs :: Int -> IO World
parseArgs argsLen = do
  args <- getArgs
  if length args < argsLen
    then do
      return $ snd $ head worlds -- just pick the head
    else do
      return $ worldFromName worlds $ head args

drawGrid :: TS.Window Int -> World -> IO ()
drawGrid (TS.Window h w) wrld = do
  let gen = mkStdGen $ wSeed wrld
  let (x,y) = (div w 4, div h 4)
  let grid = initIHexGrid gen (0, wSize wrld)
  let viewPort = ViewPort (w ,h) (x,y) 2 wrld
  mapM_ putStrLn $ finiteHexGrid viewPort grid
