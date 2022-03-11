module FiniteHexGrid(
  finiteHexGridZ
  ,finiteHexGrid
  ,oneDimNoise
  ,twoDimNoise
  ,fracZoom
  ,ViewPort(..)
  ) where

import Materials
import Utils
import InfiniteHexGrid
import qualified Data.Vector as V


povMarker = ASCIIRepr "" '@' ('x','x') ""
povBase = 100

type FiniteRow = (RowOffset, [Int])
type VGrid a = V.Vector (V.Vector a)

-- works nice w odd numbers
fracZoom :: Int -> VGrid a -> VGrid a
fracZoom z row =
  -- TODO use V.map
  V.fromList [ V.fromList [
   (row V.! fracDivideWrap zy' y) V.! fracDivideWrap zx' x
    | x <- [0 .. V.length (V.head row) - 1] ]
    | y <- [0 .. V.length row - 1] ]
  where
    zx' = fromIntegral z
    zy' = fromIntegral $ z `div` 2
    fracDivideWrap z = floor . (/z) . fromIntegral

twoDimNoise :: (Int,Int) -> (Int,Int) -> IHexGrid a -> VGrid a
twoDimNoise (w,h) (x,y) ihg =
  V.take h $ V.fromList $ ns ++ r ++ ss'
  where
    r = if y > 0 then [] else [oneDimNoise (w,x) $ row ihg]
    ss' = if y > 0 then drop (y-1) ss else ss
    ss = [ oneDimNoise (w,x) s | s <- south ihg ]
    ns = reverse . take (-y) $ [ oneDimNoise (w,x) n | n <- north ihg ]

oneDimNoise :: (Int,Int) -> IHexRow a -> V.Vector a
oneDimNoise (len,xpos) ihr =
  V.take len $ V.fromList $ w ++ pov' ++ e
  where
    pov' = if xpos > 0 then [] else [pov ihr]
    e = if xpos > 0
        then drop (xpos-1) $ east ihr
        else east ihr
    w = reverse . take (-xpos) $ west ihr

data ViewPort = ViewPort {
   size :: (Int,Int)
  ,povCoords :: (Int,Int)
  ,zoom :: Int
  ,material :: Material
} deriving (Show)

finiteHexGridZ ::  ViewPort -> IHexGrid Int -> [String]
finiteHexGridZ vp@(ViewPort _ _ zoom mat)
  | zoom == 2 = map (zoomRow2x mat . dropEndOverflow) . finiteHexGrid vp
  | zoom == 8 = map (hexCellWide8 mat . dropEndOverflow) . finiteHexGrid vp
  | otherwise = error "Not an option!"


finiteHexGrid :: ViewPort -> IHexGrid Int -> [FiniteRow]
finiteHexGrid (ViewPort (w,h) (x,y) zoom _ ) ihg =
  ns ++ [r] ++ ss
  where
    chrWidth = div w zoom -- reduce LENGHT based on wanted Horiz zoom
                          -- not usefull when zoom spans Vertically also
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
