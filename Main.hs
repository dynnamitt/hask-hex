module Main where

import InfiniteHexGrid
import FiniteHexGrid
import System.Random (getStdGen,mkStdGen)
import Data.List (zip, transpose, unfoldr)
import System.Environment
import System.Exit

heroFace = 'ツ'

data InputArgs = InputArgs {
  viewportW::Int,
  viewportH::Int
} deriving (Show)

main :: IO ()
main = do
  args' <- parseArgs 2
  drawGrid (viewportW args') (viewportH args')

-- args or death
parseArgs :: Int -> IO InputArgs
parseArgs argsLen = do
  args <- getArgs -- IO
  if length args < argsLen
    then do
      usage
      exitWith $ ExitFailure 1
    else do
      -- errorhandling ?
      return InputArgs {
        viewportW = read $ head args
      , viewportH = read $ args !! 1
     }

drawGrid :: Int -> Int -> IO ()
drawGrid maxCols maxRows = do
  let gen = mkStdGen $ wSeed world1
  let (x,y) = (div maxCols 2, div maxRows 2)
  let grid = initIHexGrid gen (0, wSize world1)
  let viewPort = ViewPort (maxCols,maxRows) (x,y) 2 world1
  mapM_ putStrLn $ finiteHexGrid viewPort grid

usage :: IO ()
usage = do
  prog <- getProgName
  putStrLn "usage:"
  putStrLn $ "  " ++ prog ++ " WIDTH HEIGHT [ZOOM] "
  putStrLn " "
  putStrLn "  Prints hexagon cells to terminal. ZOOM default = 2"
