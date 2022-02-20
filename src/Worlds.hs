module Worlds(
       world1
      ,worldMono
      ,CellVisuals(..)
      ,World(..)
      ,wSize
  ) where

import Colors

data CellVisuals = CellVisuals {
   escBefore::String
  ,cellChar ::Char
  ,escAfter ::String
} deriving (Show,Eq,Ord)

data World = World {
   wSeed :: Int
  ,biomes :: [CellVisuals]
} deriving (Show)

wSize bs = length (biomes bs) - 1

world1 = World 11 [ CellVisuals "" '█' ""
                      ,CellVisuals (fgC 3) '·' toNorm
                      ,CellVisuals "" '▒' ""
                      ,CellVisuals "" '░' ""
                      ,CellVisuals (fgC 4) '·' toNorm
                      ,CellVisuals "" '▓' ""
                    ]

worldMono = World 3031 [ CellVisuals "" '█' ""
                        ,CellVisuals "" '·' ""
                        ,CellVisuals "" '▒' ""
                        ,CellVisuals "" '░' ""
                        ,CellVisuals "" '·' ""
                        ,CellVisuals "" '▓' ""
                    ]
