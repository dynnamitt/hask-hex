module Main where

import Colors
import System.Random (mkStdGen, StdGen, getStdGen, randomR)
import Data.Char
import Data.List (zip, transpose)
import Data.Sequence (mapWithIndex,Seq)
import Data.Maybe

seed = 2022
screenLen = 80
type Hexcode = Char

-- recursive generator
randHexcodes :: StdGen -> [Hexcode]
randHexcodes g =
  let (c, g') = randomR ('A','E') g
  in c : randHexcodes g'

chunkIntoString :: Int -> [Hexcode] -> [String]
chunkIntoString lineW cs =
  let line = [take lineW cs_]
  in line ++ chunkIntoString lineW (drop lineW cs_)
  where cs_ = filter isPrint cs

-- shew with "holes"
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
  putStrLn $ nixEsc 1 ++ tColor 4 -- HI fg, yellow
  putStrLn $ "ColorTest" ++ nixEsc 0 -- restore ESC
  -- mapM_ putStrLn $ hexedRows maxCols rowStream
