module Main where

import System.Random
import Data.Char
import Data.List

seed = 2022

screenLen :: Int
screenLen = 80


main :: IO ()
main = do
  let strLineLen = screenLen `div` 2

  let chars = randChars $ mkStdGen seed
  let pureLines = chunkIntoString strLineLen chars
  let empties = take strLineLen $ repeat ' '
  let biState = cycle [True,False]
  let hexLines = map (\(xs,skew) -> interleaveSkewed xs empties skew) $ zip pureLines biState
  mapM_ putStrLn $ hexLines

-- recursive generator
randChars :: StdGen -> [Char]
randChars g =
  let (n,g') = randomR ('a','Æ') g
  in n : randChars g'

chunkIntoString :: Int -> [Char] -> [String]
chunkIntoString lineW cs =
  let line = [take lineW cs_]
  in line ++ chunkIntoString lineW (drop lineW cs_)
  where cs_ = filter isPrint cs

interleaveSkewed :: [a] -> [a] -> Bool -> [a]
interleaveSkewed x y True = interleaveLists [x, y]
interleaveSkewed x y False = interleaveLists [y, x]

interleaveLists :: [[a]] -> [a]
interleaveLists = concat . transpose
