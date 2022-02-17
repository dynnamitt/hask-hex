module Main where

import Colors
--import Influence
import InfiniteHexGrid
import System.Random (newStdGen, StdGen, getStdGen, randomR, randomRs)
import Data.Char
import Data.List (zip, transpose, unfoldr)
import Data.Sequence (mapWithIndex, Seq)
import Data.Maybe

seed = 2022
screenLen = 80

halfOf :: Int -> Int
halfOf n = div n 2

main :: IO ()
main = drawGrid2 screenLen (halfOf screenLen)

drawGrid2 :: Int -> Int -> IO ()
drawGrid2 maxCols maxRows = do
  g <- getStdGen
  let (x,y) = (3,3)
  let grid = initIHexGrid g -- rangeInput?
  let fGrid = finiteHexGrid grid (maxCols,maxRows) (x,y)
  let nicerRows = map (\r -> bgC 0 ++ r ++ toNorm) $ fromFHexRow fGrid
  let nicerRows' = fromFHexRow fGrid
  mapM_ putStrLn nicerRows'

stringRepr = "-*8/-*8/-*8" -- PutInto data w rangeInput
fromFHexRow :: [FiniteRow] -> [String]
fromFHexRow =
  map (\(off, xs) -> (offsetChr off) ++ charify xs)
  where
    offsetChr CappedEnds = " "
    offsetChr Complete = ""
    charify = map (stringRepr !!)


drawGrid :: Int -> Int -> IO ()
drawGrid maxCols maxRows = do
  g <- getStdGen
  let hexcodes = randHexcodes g
  let rowStream = chunkIntoString maxCols hexcodes
  let rows = take maxRows $ hexedRows rowStream
  let nicerRows = map (\s -> bgC 4 ++ s ++ toNorm) rows
  mapM_ putStrLn nicerRows



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
