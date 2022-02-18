module FiniteHexGrid(
       finiteHexGrid
      ,world1
      ,World(..)
      ,wSize
      ,ViewPort(..)
  ) where

import Worlds
import InfiniteHexGrid

type FiniteRow = (RowOffset, [Int])

data ViewPort = ViewPort {
   size :: (Int,Int)
  ,povCoords :: (Int,Int)
  ,zoom :: Int
  ,world :: World
} deriving (Show)


finiteHexGrid ::  ViewPort -> IHexGridCursor Int -> [String]
finiteHexGrid vp@(ViewPort _ _ zoom (World _ biomes))
  | zoom == 2 = map (zoomRow2x biomes) . finiteHexGrid' vp
  | otherwise = error "Not an option!"
  where

finiteHexGrid' :: ViewPort -> IHexGridCursor Int -> [FiniteRow]
finiteHexGrid' (ViewPort (w,h) (x,y) _ _ ) ihgCursor =
  ns ++ [r] ++ ss
  where
    r = finiteHexRow (w,x) (row ihgCursor)
    ns = reverse [ finiteHexRow (w,x) rnC | rnC <- take y $ north ihgCursor ]
    ss = [ finiteHexRow (w,x) rsC | rsC <- take (h - y) $ south ihgCursor ]

finiteHexRow :: (Int,Int) -> IHexRowCursor Int -> FiniteRow
finiteHexRow (width,xpos) ihrCursor =
   ( offset' , w ++ [pov'] ++ e )
   where
     offset' = offset ihrCursor
     offsetExtra = if offset' == Complete then 0 else 1
     pov' = pov ihrCursor
     w = reverse $ take xpos $ west ihrCursor
     e = take (width - xpos + offsetExtra) $ east ihrCursor

zoomRow2x :: [CellVisuals] -> FiniteRow -> String
zoomRow2x biomes (off, x:xs) =
   cap off x ++ middle ++ cap off (last xs)
 where
   middle = concat . map (expandCell biomes 2) $ init xs
   cap CappedEnds = expandCell biomes 1
   cap Complete = expandCell biomes 2

expandCell :: [CellVisuals] -> Int -> Int -> [Char]
expandCell biomes zoom x =
 escBefore biome ++ replicate zoom (char biome) ++ escAfter biome
 where
   biome = biomes !! x
