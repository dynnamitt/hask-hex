module FiniteHexGrid(
       finiteHexGrid
      ,ViewPort(..)
      ,zoomRow3x
  ) where

import Materials
import Colors
import InfiniteHexGrid
import Text.Printf

povMarker = ASCIIRepr "" '@' ('x','x') ""
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
  | zoom == 3 = error "OMG"
  | otherwise = error "Not an option!"

finiteHexGrid' :: ViewPort -> IHexGrid Int -> [FiniteRow]
finiteHexGrid' (ViewPort (w,h) (x,y) zoom _ ) ihg =
  ns ++ [r] ++ ss
  where
    chrWidth = div w zoom -- not good enuff
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

reprMatch :: Material -> Int -> ASCIIRepr
reprMatch mat x
   | x <= length mat = mat !! x
   | otherwise = povMarker

-- 2x Zoom
zoomRow2x :: Material -> FiniteRow -> String
zoomRow2x mat (off, x:xs) =
   cap off x ++ middle ++ cap off (last xs)
 where
   middle = concat . map (boxyCell mat 2) $ init xs
   cap CappedEnds = boxyCell mat 1 -- half
   cap Complete = boxyCell mat 2

boxyCell :: Material -> Int -> Int -> [Char]
boxyCell mat zoomY x =
  escBefore repr ++ replicate zoomY (cellChar repr) ++ escAfter repr
  where
    repr = reprMatch mat x

-- 3x Zoom
-- TODO use init if CappedEnds
zoomRow3x :: Material -> FiniteRow -> String
zoomRow3x mat (off, x:[]) = tinyHalfCell mat off x
zoomRow3x mat (off, x:xs) = tinyHalfCell mat off x ++ zoomRow3x mat (off,xs)

-- /¯ ¯\ : /¯ ¯\ # /¯ ¯\ + /¯ ¯\ x /¯ ¯\ # /¯ ¯\ + /
--   ! /¯ ¯\ : /¯ ¯\ # /¯ ¯\ + /¯ ¯\ x /¯ ¯\ # /¯ ¯\ + /
tinyHalfCell :: Material -> RowOffset -> Int -> String
tinyHalfCell mat off x
  | off == Complete = "¯\\ " ++ rc ++ " /¯ "
  | otherwise = " /¯ ¯\\ " ++ rc
  where
    rc = (:[]) . cellChar . reprMatch mat $ x
