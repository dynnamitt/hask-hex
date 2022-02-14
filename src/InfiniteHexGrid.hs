module InfiniteHexGrid(
      initIHexGrid
      ,finiteHexRow
      ,finiteHexGrid
      ,FiniteRow
      ,IHexGridCursor(..)
      ,IHexRowCursor(..)
  ) where

import System.Random (StdGen, mkStdGen, newStdGen, getStdGen, randomR,random, randomRs)

data RowOffset = Complete | CappedEnds deriving (Show,Eq,Ord)

type Point2D = (Int, Int)
type FiniteRow = (RowOffset,[Int])
randLimits = (0,9)

data IHexRowCursor a = IHexRowCursor {
  west :: [a],
  pov :: a,
  east :: [a],
  offset :: RowOffset
} deriving (Show, Eq, Ord)

type IHexRowCursorPairs a = (IHexRowCursor a,StdGen)

data IHexGridCursor a = IHexGridCursor {
  north :: [IHexRowCursorPairs a],
  row :: IHexRowCursor a,
  south :: [IHexRowCursorPairs a]
} deriving (Show, Eq)

finiteHexGrid :: IHexGridCursor Int -> Point2D -> Point2D -> [FiniteRow]
finiteHexGrid ihgCursor (width,height) (x,y) =
  ns ++ [r] ++ ss
  where
    r = finiteHexRow (row ihgCursor) width x
    ns = reverse [ finiteHexRow rnc width x | rnc <- take y $ north ihgCursor ]
    ss = [ finiteHexRow rsc width x | rsc <- take (height - y) $ south ihgCursor ]

finiteHexRow :: IHexRowCursor Int -> Int -> Int -> FiniteRow
finiteHexRow ihrCursor width xpos =
   ( offset' , w ++ [pov'] ++ e )
   where
     offset' = offset ihrCursor
     offsetExtra = if (offset' == Complete) then 0 else 1
     pov' = pov ihrCursor
     w = reverse $ take xpos $ west ihrCursor
     e = take (width - xpos + offsetExtra) $ east ihrCursor

initIHexGrid :: IO (IHexGridCursor Int)
initIHexGrid = do
  g1:g2:g3:_ <- sequence [getStdGen,newStdGen,newStdGen]
  let (row',_) = initIHexRow g1 CappedEnds
  let f' = \rowO acc -> acc ++ [initIHexRow (snd.head $ acc) rowO]
  let n = foldl f' [(row',g2)] $ cycle [Complete,CappedEnds]
  let s = foldl f' [(row',g3)] $ cycle [Complete,CappedEnds]

  --let n = initIHexRow g2 <$> cycle
  --let s = initIHexRow g3 <$> cycle [Complete,CappedEnds]
  return $ IHexGridCursor n row' s

initIHexRow :: StdGen -> RowOffset -> (IHexRowCursor Int,StdGen)
initIHexRow g1 rowO =
  (IHexRowCursor w pov' e rowO , g')
  where
    (pov', g2) = randomR randLimits g1
    (seed1, g3) = random g2 :: (Int,StdGen)
    (seed2, g') = random g3 :: (Int,StdGen)
    w = randomRs randLimits $ mkStdGen seed1
    e = randomRs randLimits $ mkStdGen seed2

-- cannot be used Lazy !!!
-- initIHexRow2 :: RowOffset -> IO (IHexRowCursor Int)
-- initIHexRow2 rowO = do
--   (pov', g2) <- randomR randLimits <$> newStdGen
--   g3 <- newStdGen
--   let w = randomRs randLimits g2
--   let e = randomRs randLimits g3
--   return $ IHexRowCursor w pov' e rowO
