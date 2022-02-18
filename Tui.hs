module Main where

import Brick

import Worlds
import FiniteHexGrid
import InfiniteHexGrid

import System.Random (getStdGen,mkStdGen)
import Data.List (intersperse)

myUniverse :: String
myUniverse =
  concat . intersperse "\n" $ finiteHexGrid viewPort grid
  where
    sizeW = (90, 90)
    zoom = 2
    gen = mkStdGen $ wSeed world1
    grid = initIHexGrid gen (0, wSize world1)
    viewPort = ViewPort sizeW (0,0) zoom world1

ui :: Widget ()
ui = str myUniverse

main :: IO ()
main = simpleMain ui
