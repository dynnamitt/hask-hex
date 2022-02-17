module Main where

import Colors
import InfiniteHexGrid
import System.Random (getStdGen)
import Data.List (zip, transpose, unfoldr)
import System.Environment
import System.Exit

screenLen = 25

biomes = "█·▒░·▓"
rRange = (0, length biomes - 1)

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
        viewportW = read $ args !! 0
      , viewportH = read $ args !! 1
     }

drawGrid :: Int -> Int -> IO ()
drawGrid maxCols maxRows = do
  g <- getStdGen
  let (x,y) = (0,0)
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

usage :: IO ()
usage = do
  prog <- getProgName
  putStrLn "usage:"
  putStrLn $ "  " ++ prog ++ " WIDTH HEIGHT [ZOOM] "
  putStrLn " "
  putStrLn "  Prints hexagon cells to terminal. ZOOM default = 2"
