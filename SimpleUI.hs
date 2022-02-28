module Main where

import InfiniteHexGrid
import Data.Maybe
import FiniteHexGrid
import Materials
import System.Random (getStdGen,mkStdGen)
import Data.List (zip, transpose, unfoldr)
import System.Environment
import qualified System.Console.Terminal.Size as TS

main :: IO ()
main = do
  matID <- parseArgs 1
  win <- TS.size
  let seed = 2022
  drawGrid ( fromJust win ) matID seed

-- args or death
parseArgs :: Int -> IO Material
parseArgs argsLen = do
  args <- getArgs
  if length args < argsLen
    then do
      return $ snd $ head materialPacks -- just pick the head
    else do
      return $ fromID materialPacks $ head args

drawGrid :: TS.Window Int -> Material -> Int -> IO ()
drawGrid (TS.Window h w) mat seed = do
  let gen = mkStdGen seed
  let materialSpan = length mat - 1
  let (x,y) = (div w 4, div h 4)
  let grid = initIHexGrid gen (0, materialSpan)
  let viewPort = ViewPort (w ,h) (x,y) 2 mat
  mapM_ putStrLn $ finiteHexGrid viewPort grid
