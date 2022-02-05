module Main where

import System.Random


main :: IO ()
main = do
  gen' <- getStdGen
  let widths = randInts gen' $ 5 - 1
  putStrLn $ "w :" ++ show widths

-- lazy , recursive
randInts :: StdGen -> Int -> [Int]
randInts g maxInt = 
  let (n,g') = randomR (0,maxInt) g
  in n : randInts g' maxInt
