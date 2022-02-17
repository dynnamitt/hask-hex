module Main where

import Colors
import InfiniteHexGrid
import System.Random (getStdGen)
import Data.List (zip, transpose, unfoldr)
import System.Environment
import System.Exit

heroFace = 'ツ'
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
        viewportW = read $ head args
      , viewportH = read $ args !! 1
     }

drawGrid :: Int -> Int -> IO ()
drawGrid maxCols maxRows = do
  g <- getStdGen
  let (x,y) = (div maxCols 2, div maxRows 2)
  let grid = initIHexGrid g rRange
  let fGrid = finiteHexGrid (maxCols,maxRows) (x,y) grid
  let rasterized = map zoomRow2x fGrid
  let nicerRows = map (\r -> bgC 6 ++ r ++ toNorm) rasterized
  mapM_ putStrLn nicerRows

zoomRow2x :: FiniteRow -> String
zoomRow2x (off, x:xs) =
    cap off x ++ middle ++ cap off (last xs)
  where
    middle = concat . map (expandCell 2) $ init xs
    cap CappedEnds = expandCell 1
    cap Complete = expandCell 2

expandCell :: Int -> Int -> [Char]
expandCell zoom x
  | x <= snd rRange = replicate zoom $ biomes !! x
  | x >= heroBase = heroFace:[]
  | otherwise = show x

usage :: IO ()
usage = do
  prog <- getProgName
  putStrLn "usage:"
  putStrLn $ "  " ++ prog ++ " WIDTH HEIGHT [ZOOM] "
  putStrLn " "
  putStrLn "  Prints hexagon cells to terminal. ZOOM default = 2"
