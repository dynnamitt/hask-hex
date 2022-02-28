module FiniteHexGrid(
       finiteHexGrid
      ,ViewPort(..)
  ) where

import Materials
import Colors
import InfiniteHexGrid

povMarker = ASCIIRepr "" '@' ""
povBase = 100

type FiniteRow = (RowOffset, [Int])

data ViewPort = ViewPort {
   size :: (Int,Int)
  ,povCoords :: (Int,Int)
  ,zoom :: Int
  ,material :: Material
} deriving (Show)


finiteHexGrid ::  ViewPort -> IHexGrid Int -> [String]
finiteHexGrid vp@(ViewPort _ _ zoom mat)
  | zoom == 2 = map (zoomRow2x mat) . finiteHexGrid' vp
  | otherwise = error "Not an option!"

finiteHexGrid' :: ViewPort -> IHexGrid Int -> [FiniteRow]
finiteHexGrid' (ViewPort (w,h) (x,y) zoom _ ) ihg =
  ns ++ [r] ++ ss
  where
    chrWidth = div w zoom
    r = finiteHexRow (chrWidth,x) povBase (row ihg)
    ns = reverse [ finiteHexRow (chrWidth,x) 0 rnC | rnC <- take y $ north ihg ]
    ss = [ finiteHexRow (chrWidth,x) 0 rsC | rsC <- take (h - y) $ south ihg ]

finiteHexRow :: (Int,Int) -> Int -> IHexRow Int -> FiniteRow
finiteHexRow (chrWidth,xpos) baseVal ihr =
   ( offset' , w ++ [pov'] ++ e )
   where
     povSpc = 1
     offset' = offset ihr
     offsetExtra = if offset' == Complete then 0 else 1
     pov' = pov ihr + baseVal
     w = reverse $ take xpos $ west ihr
     e = take ((chrWidth - povSpc) - xpos + offsetExtra) $ east ihr

zoomRow2x :: [ASCIIRepr] -> FiniteRow -> String
zoomRow2x mat (off, x:xs) =
   cap off x ++ middle ++ cap off (last xs)
 where
   middle = concat . map (showCell mat 2) $ init xs
   cap CappedEnds = showCell mat 1 -- half
   cap Complete = showCell mat 2

showCell :: [ASCIIRepr] -> Int -> Int -> [Char]
showCell mat zoomY x =
 escBefore cellV ++ replicate zoomY (cellChar cellV) ++ escAfter cellV
 where
   cellV = if x <= length mat
               then mat !! x
               else povMarker

-- z3PolyFmtPair = ("% /¯", "%¯\\ ")
--
-- zoomRow3x :: [ASCIIRepr] -> (FiniteRow,FiniteRow) -> String
-- zoomRow3x biome ((offA, a:as),(offB, b:bs)) =
--   (topFmt x , bottomFmt x): zoomRow3x s
--   where
--     completeFmt = " /¯%¯\\ % "
