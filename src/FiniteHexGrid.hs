module FiniteHexGrid(
       finiteHexGrid
      ,world1
      ,World(..)
      ,wSize
      ,ViewPort(..)
  ) where

import Worlds
import Colors
import InfiniteHexGrid

povMarker = CellVisuals "" '@' ""
povBase = 100

type FiniteRow = (RowOffset, [Int])

data ViewPort = ViewPort {
   size :: (Int,Int)
  ,povCoords :: (Int,Int)
  ,zoom :: Int
  ,world :: World
} deriving (Show)


finiteHexGrid ::  ViewPort -> IHexGrid Int -> [String]
finiteHexGrid vp@(ViewPort _ _ zoom (World _ biomes))
  | zoom == 2 = map (zoomRow2x biomes) . finiteHexGrid' vp
  | otherwise = error "Not an option!"
  where

finiteHexGrid' :: ViewPort -> IHexGrid Int -> [FiniteRow]
finiteHexGrid' (ViewPort (w,h) (x,y) zoom _ ) ihgCursor =
  ns ++ [r] ++ ss
  where
    chrWidth = div w zoom
    r = finiteHexRow (chrWidth,x) povBase (row ihgCursor)
    ns = reverse [ finiteHexRow (chrWidth,x) 0 rnC | rnC <- take y $ north ihgCursor ]
    ss = [ finiteHexRow (chrWidth,x) 0 rsC | rsC <- take (h - y) $ south ihgCursor ]

finiteHexRow :: (Int,Int) -> Int -> IHexRow Int -> FiniteRow
finiteHexRow (chrWidth,xpos) baseVal ihrCursor =
   ( offset' , w ++ [pov'] ++ e )
   where
     povLen = 1
     offset' = offset ihrCursor
     offsetExtra = if offset' == Complete then 0 else 1
     pov' = pov ihrCursor + baseVal
     w = reverse $ take xpos $ west ihrCursor
     e = take ((chrWidth - povLen) - xpos + offsetExtra) $ east ihrCursor

zoomRow2x :: [CellVisuals] -> FiniteRow -> String
zoomRow2x biomes (off, x:xs) =
   cap off x ++ middle ++ cap off (last xs)
 where
   middle = concat . map (showSquareCell biomes 2) $ init xs
   cap CappedEnds = showSquareCell biomes 1 -- half
   cap Complete = showSquareCell biomes 2

showSquareCell :: [CellVisuals] -> Int -> Int -> [Char]
showSquareCell biomes zoom x =
 escBefore cellV ++ replicate zoom (cellChar cellV) ++ escAfter cellV
 where
   cellV = if x <= length biomes
               then biomes !! x
               else povMarker
