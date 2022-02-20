module Main where

import Brick (Widget, simpleMain, (<+>), str, withBorderStyle)
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)

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
    grid = move East $ initIHexGrid gen (0, wSize w')
    viewPort = ViewPort sizeW (10,10) zoom w'

ui :: Widget ()
ui =
  withBorderStyle unicode $
  borderWithLabel (str "Hello!") $
  (center (str myUniverse) <+> vBorder <+> center (str "Right"))
  --str myUniverse

main :: IO ()
main = simpleMain ui
