module Main where

import System.Random
import Data.Char

seed = 2022



main :: IO ()
main = do

  let cs = randChars $ mkStdGen seed
  mapM_ putStrLn $ collectAsString 40 cs

-- recursive generator
randChars :: StdGen -> [Char]
randChars g =
  let (n,g') = randomR ('a','Æ') g
  in n : randChars g'

collectAsString :: Int -> [Char] -> [String]
collectAsString lineW cs =
  let line = [take lineW cs_]
  in line ++ collectAsString lineW (drop lineW cs)
  where cs_ = filter isPrint cs
