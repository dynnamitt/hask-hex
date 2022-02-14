module InfiniteHexGrid(
      initIHexGrid

      ,initIHexRow2
      ,RowOffset(..)

      ,showHexRow
      ,showHexGrid
      ,FiniteRow
      ,IHexGridCursor(..)
      ,IHexRowCursor(..)
  ) where

import System.Random (StdGen, mkStdGen, newStdGen, getStdGen, randomR,random, randomRs)

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
  g1:g2:g3:_ <- sequence [getStdGen,newStdGen,newStdGen]
  let row' = initIHexRow g1 CappedEnds
  let n = initIHexRow g2 <$> cycle [Complete,CappedEnds]
  let s = initIHexRow g3 <$> cycle [Complete,CappedEnds]
  return $ IHexGridCursor n row' s


initIHexRow :: StdGen -> RowOffset -> IHexRowCursor Int
initIHexRow g1 rowO =
  IHexRowCursor w pov' e rowO
  where
    (pov', g2) = randomR randLimits g1
    (seed1, g3) = random g2 :: (Int,StdGen)
    (seed2, g') = random g3 :: (Int,StdGen)
    w = randomRs randLimits $ mkStdGen seed1
    e = randomRs randLimits $ mkStdGen seed2

initIHexRow2 :: RowOffset -> IO (IHexRowCursor Int)
initIHexRow2 rowO = do
  (pov', g2) <- randomR randLimits <$> newStdGen
  g3 <- newStdGen
  let w = randomRs randLimits g2
  let e = randomRs randLimits g3
  return $ IHexRowCursor w pov' e rowO
