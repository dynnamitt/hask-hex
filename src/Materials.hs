module Materials(
       materialPacks
      ,fromID
      ,ASCIIRepr(..)
      ,Material
  ) where

 --   . ____    .
 --  /           \
 -- .             .
 --  \           /
 --    . _____  .

import Colors

data ASCIIRepr = ASCIIRepr {
   escBefore::String
  ,cellChar ::Char
  ,escAfter ::String
} deriving (Show,Eq,Ord)

type Material = [ASCIIRepr]

materialPacks = [
  ("256",col256),
  ("16",col16),
  ("w1",w1),
  ("mono",mono),
  ("bi",bi),
  ("bi2",bi2),
  ("bi3",bi3),
  ("bi4",bi4)
  ]

fromID :: [(String,Material)] -> String -> Material
fromID [] _ = error $ "NOT FOUND, use one of: " ++ (show $ map fst materialPacks)
fromID ((n,w):xs) name
            | name == n = w
            | otherwise = fromID xs name


col16 = [ ASCIIRepr (bg16 True x) '░' toNorm | x <- [0..7] ]

col256 = [ ASCIIRepr ((fg16 False 0) <> (bg256 x)) c toNorm
                    | (x,c) <- zip [8..255] $ cycle ":x#+,-" ]

w1 =  [ ASCIIRepr "" '█' ""
        ,ASCIIRepr (fg16 True 3) '·' toNorm
        ,ASCIIRepr "" '▒' ""
        ,ASCIIRepr "" '▒' ""
        ,ASCIIRepr (fg16 False 4) '·' toNorm
        ,ASCIIRepr "" '▓' ""
      ]

bi = [ ASCIIRepr "" '█' ""
      ,ASCIIRepr "" '·' ""
    ]

bi2 = [ ASCIIRepr "" '█' ""
        ,ASCIIRepr "" '·' ""
        ,ASCIIRepr "" '·' ""
    ]

bi3 = [ ASCIIRepr "" '█' ""
        ,ASCIIRepr "" '·' ""
        ,ASCIIRepr "" '·' ""
        ,ASCIIRepr "" '·' ""
    ]

bi4 = [ ASCIIRepr "" '-' ""
        ,ASCIIRepr "" '!' ""
        ,ASCIIRepr "" '█' ""
        ,ASCIIRepr "" '█' ""
        ,ASCIIRepr "" '█' ""
        ,ASCIIRepr "" '█' ""
        ,ASCIIRepr "" '█' ""
    ]

mono = [ ASCIIRepr "" '█' ""
        ,ASCIIRepr "" '·' ""
        ,ASCIIRepr "" '▒' ""
        ,ASCIIRepr "" '░' ""
        ,ASCIIRepr "" '·' ""
        ,ASCIIRepr "" '▓' ""
    ]
