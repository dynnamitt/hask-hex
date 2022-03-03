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
   escBefore:: String
  ,cellChar :: Char
  ,altChars :: (Char,Char)
  ,escAfter :: String
} deriving (Show,Eq,Ord)

alts = ('ª', '₀')

type Material = [ASCIIRepr]

materialPacks = [
  ("256",col256),
  ("16",col16),
  ("w1",w1),
  ("mono",mono),
  ("mono2",mono2),
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

col16 = [ ASCIIRepr (bg16 True x) '░' alts toNorm | x <- [0..7] ]

col256 = [ ASCIIRepr ((fg16 False 0) <> (bg256 x)) c alts toNorm
                    | (x,c) <- zip [8..255] $ cycle ":x#+,-" ]

w1 =  [ ASCIIRepr "" '█' alts ""
        ,ASCIIRepr (fg16 True 3) '·' alts toNorm
        ,ASCIIRepr "" '▒' alts ""
        ,ASCIIRepr "" '▒' alts ""
        ,ASCIIRepr (fg16 False 4) '·' alts toNorm
        ,ASCIIRepr "" '▓' alts ""
      ]

bi = [ ASCIIRepr "" '█' alts ""
      ,ASCIIRepr "" '·' alts ""
    ]

bi2 = [ ASCIIRepr "" '█' alts ""
        ,ASCIIRepr "" '·' alts ""
        ,ASCIIRepr "" '·' alts ""
    ]

bi3 = [ ASCIIRepr "" '█' alts ""
        ,ASCIIRepr "" '·' alts ""
        ,ASCIIRepr "" '·' alts ""
        ,ASCIIRepr "" '·' alts ""
    ]

bi4 = [ ASCIIRepr "" '-' alts ""
        ,ASCIIRepr "" '!' alts ""
        ,ASCIIRepr "" '█' alts ""
        ,ASCIIRepr "" '█' alts ""
        ,ASCIIRepr "" '█' alts ""
        ,ASCIIRepr "" '█' alts ""
        ,ASCIIRepr "" '█' alts ""
    ]

mono = [ ASCIIRepr "" '█' alts ""
        ,ASCIIRepr "" '·' alts ""
        ,ASCIIRepr "" '▒' alts ""
        ,ASCIIRepr "" '░' alts ""
        ,ASCIIRepr "" '·' alts ""
        ,ASCIIRepr "" '▓' alts ""
    ]

mono2 = [ ASCIIRepr "" '♉' alts ""
          ,ASCIIRepr "" '·' alts ""
          ,ASCIIRepr "" '▃' alts ""
          ,ASCIIRepr "" '∎' alts ""
          ,ASCIIRepr "" '·' alts ""
          ,ASCIIRepr "" '♒' alts ""
    ]
