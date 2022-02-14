module InfiniteHexGrid(
      initIHexGrid
      ,showHexRow
      ,showHexGrid
      ,FiniteRow
      ,IHexGridCursor(..)
      ,IHexRowCursor(..)
  ) where

import System.Random (StdGen, newStdGen, getStdGen, randomR, randomRs)

data RowOffset = Complete | CappedEnds deriving (Show,Eq,Ord)

type Point2D = (Int, Int)
type GenTriple = (StdGen,StdGen,StdGen)
type FiniteRow = (RowOffset,[Int])
randLimits = (0,9)

data IHexRowCursor a = IHexRowCursor {
  west :: [a],
  pov :: a,
  east :: [a],
  offset :: RowOffset
} deriving (Show, Eq, Ord)



data IHexGridCursor a = IHexGridCursor {
  north :: [IHexRowCursor a],
  row :: IHexRowCursor a,
  south :: [IHexRowCursor a]
} deriving (Show, Eq, Ord)

showHexGrid :: IHexGridCursor Int -> Point2D -> Point2D -> [FiniteRow]
showHexGrid gcursor (width,height) (x,y) =
  ns ++ [r] ++ ss
  where
    r = showHexRow (row gcursor) width x
    ns = reverse [ showHexRow rnc width x | rnc <- take y $ north gcursor ]
    ss = [ showHexRow rsc width x | rsc <- take (height - y) $ south gcursor ]

showHexRow :: IHexRowCursor Int -> Int -> Int -> FiniteRow
showHexRow rcursor width xpos =
   ( offset' , w ++ [pov'] ++ e )
   where
     offset' = offset rcursor
     offsetExtra = if (offset' == Complete) then 0 else 1
     pov' = pov rcursor
     w = reverse $ take xpos $ west rcursor
     e = take (width - xpos + offsetExtra) $ east rcursor

initIHexGrid :: IO (IHexGridCursor Int)
initIHexGrid = do
  gt1:gt2:xs <- sequence [genTriplet,genTriplet,genTriplet]
  let gt3 = head xs
  let row' = initIHexRow gt1 (Just 99) CappedEnds
  let n = initIHexRow gt2 Nothing <$> cycle [Complete,CappedEnds]
  let s = initIHexRow gt3 Nothing <$> cycle [Complete,CappedEnds]
  return $ IHexGridCursor n row' s

genTriplet :: IO GenTriple
genTriplet = do
  x:y:z <- sequence $ take 3 $ cycle [getStdGen,newStdGen]
  return (x,y,head z)

initIHexRow :: GenTriple -> Maybe Int -> RowOffset -> IHexRowCursor Int
initIHexRow (g1,g2,g3) povOverride rowO =
  IHexRowCursor w (getPov povOverride) e rowO
  where
    getPov Nothing = pov'
    getPov (Just n) = n
    (pov',_) = randomR randLimits g1
    w = randomRs randLimits g2
    e = randomRs randLimits g3
