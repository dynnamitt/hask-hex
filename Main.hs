module Main where

import System.Random
import Data.Char

seed = 2022



main :: IO ()
main = do

  let cs = randChars $ mkStdGen seed
  mapM_ putStrLn $ newLines 40 cs

-- recursive generator
randChars :: StdGen -> [Char]
randChars g =
  let (n,g') = randomR ('a','Æ') g
  in n : randChars g'


newLines :: Int -> [Char] -> [String]
newLines lineW cs =
  let line = [take lineW cs]
  in line ++ newLines lineW (drop lineW cs)
