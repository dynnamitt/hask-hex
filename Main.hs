module Main where

import System.Random (mkStdGen, StdGen, randomR)
import Data.Char
import Data.List (zip, transpose)
import Data.Sequence (mapWithIndex)
import Data.Maybe

seed = 2022
screenLen = 80

type Hexcode = Char

{-
    A B
   S * ?
    ? ?
-}

data HexCell = HexCell {
  branchedFrom::Influencer, -- what is the cost?
  branchedTimes::Int,
  xPos::Int,
  hexcode::Hexcode
} deriving (Show)

data Influencer = A HexCell
                | B HexCell
                | Side HexCell
                | NoInflux
                deriving (Show)

-- hexedCellRow Nothing currRow =
--   currRow':hexedCellRow
--   where
--     currRow' = mapWithIndex (\pos c -> HexCell NoInflux 0 pos c) currRow
--     fn' itm acc = acc ++ [itm] -- place back into same order

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
  mapM_ putStrLn $ hexedRows maxCols rowStream
