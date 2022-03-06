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
  (matID,zoom) <- parseArgs 2
  win <- TS.size
  let seed = 2022
  drawGrid ( fromJust win ) matID seed zoom

-- args
parseArgs :: Int -> IO (Material,Int)
parseArgs argsLen = do
  args <- getArgs
  if length args < argsLen
    then do
      let mat = snd $ head materialPacks -- just pick the head
      return (mat,2)
    else do
      let mat = fromID materialPacks $ head args
      let zoom = read . head . tail $ args
      return (mat,zoom)

drawGrid :: TS.Window Int -> Material -> Int -> Int -> IO ()
drawGrid (TS.Window h w) mat seed zoom = do
  let gen = mkStdGen seed
  let materialSpan = length mat - 1
  let (x,y) = (5, 5)
  let grid = initIHexGrid gen (0, materialSpan)
  let viewPort = ViewPort (w ,h) (x,y) zoom mat
  mapM_ putStrLn $ finiteHexGridZ viewPort grid
