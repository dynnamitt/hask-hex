{-# LANGUAGE NumericUnderscores #-}

module Main where

import InfiniteHexGrid
import Data.Maybe
import Utils
import FiniteHexGrid
import Materials
import System.Random (getStdGen,mkStdGen)
import Data.List (zip, transpose, unfoldr, intercalate)
import System.Environment
import qualified System.Console.Terminal.Size as TS

main :: IO ()
main = do
  zoom <- parseArgs
  win <- TS.size
  plotGrid ( fromJust win ) zoom

-- args
parseArgs :: IO Int
parseArgs = do
  args <- getArgs
  if null args
    then return 2
    else return $ read . head $ args


max' = 9_000 :: Int
seed = 2_023

plotGrid :: TS.Window Int -> Int -> IO ()
plotGrid (TS.Window h w) zoom = do
  let gen = mkStdGen seed
  let iGrid = initIHexGrid gen (0, max')
  let dim' = dim (w,h) zoom
  let fGrid = twoDimNoise dim' (0,0) iGrid
  mapM_ putStrLn $ hZoom zoom $ map (plotPixel zoom) fGrid
  putStrLn $  "Summary z:" <> show zoom <> ", h:" <> show h <> ", dim':" <> show dim'


plotPixel :: Int -> [Int] -> String
plotPixel zoom xs =
  intercalate "" $ map pixel colors
  where
    pixel c = bg256 c ++ replicate zoom ' ' ++ toNorm
    colors = map f' xs
    f' = (+cBase) . round . (*cLen) . frac max'
    cLen = fromIntegral $ length grayscale -1
    cBase = head grayscale

hZoom :: Int -> [String] -> [String]
hZoom z (x:[]) = replicate (z `div` 2) x
hZoom z (x:xs) = replicate (z `div` 2) x ++ hZoom z xs

dim :: (Int,Int) -> Int -> (Int,Int)
dim (w,h) zoom = (w `div` zoom, h `div` (zoom `div` 2))

-- drawGrid :: TS.Window Int -> Material -> Int -> Int -> IO ()
-- drawGrid (TS.Window h w) mat seed zoom = do
--   let gen = mkStdGen seed
--   let materialSpan = length mat - 1
--   let (x,y) = (5, 5)
--   let grid = initIHexGrid gen (0, materialSpan)
--   let viewPort = ViewPort (w ,h) (x,y) zoom mat
--   mapM_ putStrLn $ finiteHexGridZ viewPort grid
