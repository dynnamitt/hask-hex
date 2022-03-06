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
  (matID, zoom) <- parseArgs 2
  win <- TS.size
  plotGrid ( fromJust win ) zoom

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

grayScales = (232,255)
max' = 9_000 :: Int
seed = 2_023

plotGrid :: TS.Window Int -> Int -> IO ()
plotGrid (TS.Window h w) zoom = do
  let gen = mkStdGen seed
  let iGrid = initIHexGrid gen (0, max')
  let dim' = dim (w,h) zoom
  let fGrid = twoDimNoise dim' (0,0) iGrid
  mapM_ putStrLn $ map (plotPixel zoom) fGrid

plotPixel :: Int -> [Int] -> String
plotPixel zoom xs =
  intercalate "" $ map pixel colors
  where
    pixel c = bg256 c ++ replicate zoom ' ' ++ toNorm
    colors = map f' xs
    f' = (+grayBase) . round . (*grayLen) . frac max'
    grayLen = fromIntegral $ (snd grayScales) - grayBase
    grayBase = fst grayScales

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
