module Main where

import Colors
--import Influence
import InfiniteHexGrid
import System.Random (newStdGen, StdGen, getStdGen, randomR, randomRs)
import Data.Char
import Data.List (zip, transpose)
import Data.Sequence (mapWithIndex, Seq)
import Data.Maybe

seed = 2022
screenLen = 18 * 2

main :: IO ()
main = drawGrid screenLen screenLen


drawGrid :: Int -> Int -> IO ()
drawGrid maxCols maxRows = do
  g <- getStdGen
  let hexcodes = randHexcodes g
  let rowStream = chunkIntoString maxCols hexcodes
  let rows = take maxRows $ hexedRows rowStream
  let nicerRows = map (\s -> bgC 4 ++ s ++ toNorm) rows
  mapM_ putStrLn nicerRows


stringRepr = "-*8/zABCd"
-- Hexcode bounds
randLimits = (0,5)

type Hexcode = Int

-- recursive generator
randHexcodes :: StdGen -> [Hexcode]
randHexcodes g = randomRs randLimits g

chunkIntoString :: Int -> [Hexcode] -> [String]
chunkIntoString lineW cs =
  let line = map (stringRepr !!) $ take lineW cs -- concatMap (wrapC "X") $
  in line : chunkIntoString lineW (drop lineW cs)

-- shew with "-1"
hexedRows :: [String] -> [String]
hexedRows rows =
  map (uncurry interleaveSkewed) bipolarRows
  where
    bipolarRows = zip rows $ cycle [True,False]
    spc n = replicate  n ' '
    interleaveSkewed row True = interleaveLists [row, spc $ length row]
    interleaveSkewed row False = interleaveLists [spc $ length row, row]

interleaveLists :: [[a]] -> [a]
interleaveLists = concat . transpose
