module Main where

import Brick

import Worlds
import FiniteHexGrid
import InfiniteHexGrid

import System.Random (getStdGen,mkStdGen)
import Data.List (intersperse)

w' = worldMono

myUniverse :: String
myUniverse =
  concat . intersperse "\n" $ finiteHexGrid viewPort grid
  where
    sizeW = (222, 222)
    zoom = 2
    gen = mkStdGen $ wSeed w'
    grid = initIHexGrid gen (0, wSize w')
    viewPort = ViewPort sizeW (10,10) zoom w'

ui :: Widget ()
ui = str myUniverse

main :: IO ()
main = simpleMain ui
