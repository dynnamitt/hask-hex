{-# LANGUAGE NumericUnderscores #-}

module Main where

import InfiniteHexGrid
import Data.Maybe
import Utils
import VectorGridPattern
import Materials
import System.Environment
import System.Random (getStdGen,mkStdGen)
import Data.List (zip, transpose, unfoldr, intercalate)
import qualified Data.Vector as V
import System.Console.ANSI
import qualified System.Console.Terminal.Size as TS
import System.IO (hSetEcho, getChar, hReady, stdin,stdout, Handle)


type World a = (Int,Int,Int,Transformation a Int,IHexGrid a)

hi = 3_000 :: Int
defaultSeed = 2_023

-- https://stackoverflow.com/questions/19285691/how-do-i-write-a-game-loop-in-haskell
main :: IO ()
main = loop initWorld
  where loop w = w >>= displayWorld >>
                loop (w >>= gameLoop)

initWorld :: RealFrac a => IO (World a)
initWorld =  do
  hSetEcho stdin False                        -- Not working ?
  seed <- parseArgs <$> getArgs
  (TS.Window h w) <- fromJust <$> TS.size
  let gen = mkStdGen seed
  let igrid = initIHexGrid gen (0, hi)
  return (h,w,8,smooth,igrid)

displayWorld :: RealFrac a => World a -> IO ()
displayWorld (h,w,z,filterFn,igrid) = do
  let fGrid = twoDimNoise (w,h) (3,3) igrid
  setCursorPosition 0 0
  mapM_ putStr $ render z filterFn fGrid

gameLoop :: World a -> IO (World a)
gameLoop (h,w,z,filterFn,igrid) = do
  ch <- ifReadyDo stdin getChar
  let incrZ = 1 -- if ch == Nothing then 0 else 2
  return (h,w,z+incrZ,filterFn,igrid)

ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing

-- args
parseArgs :: [String] -> Int
parseArgs [] = defaultSeed
parseArgs (x:xs)
  | null xs = read x
  | otherwise = read x
