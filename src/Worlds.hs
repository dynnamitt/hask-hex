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

worlds = [("w1",w1),("mono",mono),("bi",bi)]

worldFromName :: [(String,World)] -> String -> World
worldFromName [] _ = error "OOPS, have not defined that world yet!!"
worldFromName ((n,w):xs) name
            | name == n = w
            | otherwise = worldFromName xs name


w1 = World 11 [ CellVisuals "" '█' ""
                      ,CellVisuals (fgC 3) '·' toNorm
                      ,CellVisuals "" '▒' ""
                      ,CellVisuals "" '░' ""
                      ,CellVisuals (fgC 4) '·' toNorm
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
