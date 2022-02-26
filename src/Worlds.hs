module Worlds(
       worlds
      ,worldFromName
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

worlds = [("256",col256),("wow",col16),("w1",w1),("mono",mono),("bi",bi)]

worldFromName :: [(String,World)] -> String -> World
worldFromName [] _ = error "OOPS, have not defined that world yet!!"
worldFromName ((n,w):xs) name
            | name == n = w
            | otherwise = worldFromName xs name


col16 = World 11 [ CellVisuals (bg16 True bg) ':' toNorm | bg <- [0..7] ]

col256 = World 11 [ CellVisuals (bg256 bg) '+' toNorm | bg <- [0..255] ]


w1 = World 11 [ CellVisuals "" '█' ""
                ,CellVisuals (fg16 True 3) '·' toNorm
                ,CellVisuals "" '▒' ""
                ,CellVisuals "" '░' ""
                ,CellVisuals (fg16 False 4) '·' toNorm
                ,CellVisuals "" '▓' ""
              ]

bi = World 3031 [ CellVisuals "" '█' ""
                  ,CellVisuals "" '·' ""
                ]

mono = World 3031 [ CellVisuals "" '█' ""
                    ,CellVisuals "" '·' ""
                    ,CellVisuals "" '▒' ""
                    ,CellVisuals "" '░' ""
                    ,CellVisuals "" '·' ""
                    ,CellVisuals "" '▓' ""
                ]
