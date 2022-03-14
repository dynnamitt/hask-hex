{-# LANGUAGE NumericUnderscores #-}

module Main where

import InfiniteHexGrid
import Data.Maybe
import Utils
import VectorGridPattern
import Materials
import System.Random (getStdGen,mkStdGen)
import Data.List (zip, transpose, unfoldr, intercalate)
import qualified Data.Vector as V
import System.Environment
import qualified System.Console.Terminal.Size as TS


main :: IO ()
main = do
  win <- TS.size
  args <- getArgs
  let (zoom, fn) = parseArgs args
  plotGrid ( fromJust win ) zoom fn

-- args
parseArgs :: RealFrac a => [String] -> (Int, Transformation a Int)
parseArgs [] = (2,smooth)
parseArgs (x:xs)
  | null xs = (read x,smooth)
  | otherwise = (read x,cubic)

hi = 1_000 :: Int
seed = 2_023

plotGrid :: RealFrac a => TS.Window Int -> Int -> Transformation a Int -> IO ()
plotGrid (TS.Window h w) zoom transFn = do
  let gen = mkStdGen seed
  let iGrid = initIHexGrid gen (0, hi)
  let fGrid = twoDimNoise (w,h) (3,3) iGrid
  putStrLn $  "Summary z:" <> show zoom <> ", h:" <> show h <> ", w:" <> show w
  mapM_ putStrLn $ render zoom transFn fGrid
