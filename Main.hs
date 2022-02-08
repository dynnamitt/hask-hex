module Main where

import Colors
--import Influence
import System.Random (mkStdGen, StdGen, getStdGen, randomR)
import Data.Char
import Data.List (zip, transpose)
import Data.Sequence (mapWithIndex,Seq)
import Data.Maybe

seed = 2022
screenLen = 80
type Hexcode = Int
rLow = 1 :: Int
rHi = 9 :: Int


-- recursive generator
randHexcodes :: StdGen -> [Hexcode]
randHexcodes g =
  let (c, g') = randomR (rLow, rHi) g
  in c : randHexcodes g'

chunkIntoString :: Int -> [Hexcode] -> [String]
chunkIntoString lineW cs =
  let line = [concatMap (wrapC "X") $ take lineW cs]
  in line ++ chunkIntoString lineW (drop lineW cs)

-- shew with "-1"
hexedRows :: Int -> [String] -> [String]
hexedRows maxCols rows =
  let holes = replicate maxCols ' '
      bipolarRows = zip rows $ cycle [True,False]
  in map (\(row,skew) -> interleaveSkewed row holes skew) bipolarRows
  where
    interleaveSkewed x y True = interleaveLists [x, y]
    interleaveSkewed x y False = interleaveLists [y, x]

interleaveLists :: [[a]] -> [a]
interleaveLists = concat . transpose

main :: IO ()
main = do
  let maxCols = screenLen `div` 2
  let hexcodes = randHexcodes $ mkStdGen seed
  let rowStream = chunkIntoString maxCols hexcodes

  mapM_ putStrLn $ take 1000 $ rowStream
