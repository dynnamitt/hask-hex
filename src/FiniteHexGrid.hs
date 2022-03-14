module FiniteHexGrid(
  finiteHexGridZ
  ,finiteHexGrid
  ,ViewPort(..)
  ) where

import Materials
import Utils
import InfiniteHexGrid

povMarker = ASCIIRepr "" '@' ('x','x') ""
povBase = 100

type FiniteRow = (RowOffset, [Int])


data ViewPort = ViewPort {
   size :: (Int,Int)
  ,povCoords :: (Int,Int)
  ,zoom :: Int
  ,material :: Material
} deriving (Show)

finiteHexGridZ ::  ViewPort -> IHexGrid a -> [String]
finiteHexGridZ vp@(ViewPort _ _ zoom mat)
  | zoom == 2 = map (zoomRow2x mat . dropEndOverflow) . finiteHexGrid vp
  | zoom == 8 = map (hexCellWide8 mat . dropEndOverflow) . finiteHexGrid vp
  | otherwise = error "Not an option!"

finiteHexGrid :: ViewPort -> IHexGrid a -> [FiniteRow]
finiteHexGrid (ViewPort (w,h) (x,y) zoom _ ) ihg =
  ns ++ [r] ++ ss
  where
    chrWidth = div w zoom -- reduce LENGHT based on wanted Horiz zoom
                          -- not usefull when zoom spans Vertically also
    r = finiteHexRow (chrWidth,x) povBase (row ihg)
    ns = reverse [ finiteHexRow (chrWidth,x) 0 rnC | rnC <- take y $ north ihg ]
    ss = [ finiteHexRow (chrWidth,x) 0 rsC | rsC <- take (h - y) $ south ihg ]

finiteHexRow :: (Int,Int) -> Int -> IHexRow a -> FiniteRow
finiteHexRow (chrWidth,xpos) baseVal ihr =
   ( offset' , w ++ [pov'] ++ e )
   where
     povSpc = 1
     offset' = offset ihr
     offsetExtra = if offset' == Complete then 0 else 1
     pov' = pov ihr + baseVal
     w = reverse $ take xpos $ west ihr
     e = take ((chrWidth - povSpc) - xpos + offsetExtra) $ east ihr


dropEndOverflow :: FiniteRow -> FiniteRow
dropEndOverflow (CappedEnds,xs) = (CappedEnds,init xs)
dropEndOverflow r@(_,_) = r


-- 2x Zoom
zoomRow2x :: Material -> FiniteRow -> String
zoomRow2x mat (off, x:xs) =
   cap off x ++ middle ++ cap off (last xs)
 where
   middle = concat . map (boxyCell mat 2) $ init xs
   cap CappedEnds = boxyCell mat 2 -- use 1/2 to Offset
   cap Complete = boxyCell mat 2


boxyCell :: Material -> Int -> Int -> [Char]
boxyCell mat zoomY x =
  escBefore repr ++ rc ++ escAfter repr
  where
    repr = reprMatch mat x
    rc = padd ++ (cellChar repr):[]
    padd = replicate (zoomY - 1) ' '


reprMatch :: Material -> Int -> ASCIIRepr
reprMatch mat x
   | x <= length mat = mat !! x
   | otherwise = povMarker

-- 8x Zoom
hexCellWide8 :: Material -> FiniteRow -> String
hexCellWide8 mat (off, x:[]) = tinyHalfCell mat off x
hexCellWide8 mat (off, x:xs) = tinyHalfCell mat off x ++ hexCellWide8 mat (off,xs)


-- /¯ ¯\ : /¯ ¯\ # /¯ ¯\ + /¯ ¯\ x /¯ ¯\ # /¯ ¯\ + /
--   ! /¯ ¯\ : /¯ ¯\ # /¯ ¯\ + /¯ ¯\ x /¯ ¯\ # /¯ ¯\ + /
tinyHalfCell :: Material -> RowOffset -> Int -> String
tinyHalfCell mat off x
  | off == Complete = " ▔▚ " ++ rc ++ " ▞▔"
  | otherwise = rc ++ " ▞▔ ▔▚ "
  where
    rc = (:[]) . cellChar . reprMatch mat $ x
