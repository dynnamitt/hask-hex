module Main where

import System.Random
import Data.Char
import Data.List

seed = 2022
screenLen = 80

main :: IO ()
main = do
  let maxCols = screenLen `div` 2
  let charStream = randChars $ mkStdGen seed
  let rowStream = chunkIntoString maxCols charStream
  mapM_ putStrLn $ hexedRows maxCols rowStream

-- recursive generator
randChars :: StdGen -> [Char]
randChars g =
  let (c, g') = randomR ('A','L') g
  in c : randChars g'

chunkIntoString :: Int -> [Char] -> [String]
chunkIntoString lineW cs =
  let line = [take lineW cs_]
  in line ++ chunkIntoString lineW (drop lineW cs_)
  where cs_ = filter isPrint cs

-- shew with "holes"
hexedRows :: Int -> [String] -> [String]
hexedRows maxCols rows =
  let holes = take maxCols $ repeat ' '
      bipolarRows = zip rows $ cycle [True,False]
  in map (\(row,skew) -> interleaveSkewed row holes skew) bipolarRows
  where
    interleaveSkewed x y True = interleaveLists [x, y]
    interleaveSkewed x y False = interleaveLists [y, x]

interleaveLists :: [[a]] -> [a]
interleaveLists = concat . transpose
