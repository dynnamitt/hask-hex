module Main where

import Colors
import Influence (Hexcode)
import System.Random (mkStdGen, StdGen, getStdGen, randomR)
import Data.Char
import Data.List (zip, transpose)
import Data.Sequence (mapWithIndex,Seq)
import Data.Maybe

seed = 2022
screenLen = 80

-- Hexcode bounds
rLow = 1 :: Int
rHi = 9 :: Int

stringRepr = "xyzABCd-*."


-- recursive generator
randHexcodes :: StdGen -> [Hexcode]
randHexcodes g =
  let (c, g') = randomR (rLow, rHi) g
  in c : randHexcodes g'


chunkIntoString :: Int -> [Hexcode] -> [String]
chunkIntoString lineW cs =
  let line = map (stringRepr !!) $ take lineW cs -- concatMap (wrapC "X") $
  in [line] ++ chunkIntoString lineW (drop lineW cs)

-- shew with "-1"
hexedRows :: [String] -> [String]
hexedRows rows =
  map (\(row, skew) -> interleaveSkewed row skew) bipolarRows
  where
    bipolarRows = zip rows $ cycle [True,False]
    spc n = replicate  n ' '
    interleaveSkewed row True = interleaveLists [row, spc $ length row]
    interleaveSkewed row False = interleaveLists [spc $ length row, row]

interleaveLists :: [[a]] -> [a]
interleaveLists = concat . transpose

main :: IO ()
main = do
  let maxCols = screenLen `div` 2
  let hexcodes = randHexcodes $ mkStdGen seed
  let rowStream = chunkIntoString maxCols hexcodes
  --putStrLn $ nixEsc 1 ++ tColor 4 -- HI fg, yellow
  --putStrLn $ "ColorTest" ++ nixEsc 0 -- restore ESC
  mapM_ putStrLn $ hexedRows rowStream
