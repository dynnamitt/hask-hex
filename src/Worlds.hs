module Worlds(
       world1
      ,CellVisuals(..)
      ,World(..)
      ,wSize
  ) where

import Colors

data CellVisuals = CellVisuals {
   escBefore::String
  ,char ::Char
  ,escAfter ::String
} deriving (Show,Eq,Ord)

data World = World {
   wSeed :: Int
  ,biomes :: [CellVisuals]
} deriving (Show)

wSize bs = length (biomes bs) - 1

world1 = World 2021 [ CellVisuals "" '█' ""
                      ,CellVisuals (fgC 3) '·' toNorm
                      ,CellVisuals "" '▒' ""
                      ,CellVisuals "" '░' ""
                      ,CellVisuals (fgC 4) '·' toNorm
                      ,CellVisuals "" '▓' ""
                    ]
