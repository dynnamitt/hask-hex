module InfiniteHexGrid(
      initIHexGrid
      ,initIHexRow
      ,finiteHexRow
      ,finiteHexGrid
      ,FiniteRow
      ,RowOffset(..)
      ,IHexGridCursor(..)
      ,IHexRowCursor(..)
  ) where

import Data.List (unfoldr)
import System.Random (
  RandomGen, uniform, uniformR, mkStdGen)

type Point2D = (Int, Int)
type FiniteRow = (RowOffset,[Int])
randLimits = (0,9)
randomSalt = 42

data RowOffset = Complete | CappedEnds deriving (Show,Eq,Ord)

-- Make monoid, we need an empty/identity for fold?
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


initIHexGrid :: RandomGen g => g -> IHexGridCursor Int
initIHexGrid g =
  IHexGridCursor n row' s
  where
    s0:s1:northSeeds = unfoldr (Just . uniform) g :: [Int]
    southSeeds = unfoldr (Just . uniform) $ mkStdGen s0 :: [Int]
    row' = initIHexRow (mkStdGen s1) CappedEnds
    offsets = cycle [Complete,CappedEnds]
    n = map (\(s,off) -> initIHexRow (mkStdGen s) off) $ zip northSeeds offsets
    s = map (\(s,off) -> initIHexRow (mkStdGen s) off) $ zip southSeeds offsets


initIHexRow :: RandomGen g => g -> RowOffset -> IHexRowCursor Int
initIHexRow g1 offset =
  IHexRowCursor w pov' e offset
  where
    seed0:seed1:_ = unfoldr (Just . uniform) g1 :: [Int]
    (pov', g2) = uniformR randLimits $ mkStdGen seed0
    w = unfoldr (Just . uniformR randLimits) $ g2
    e = unfoldr (Just . uniformR randLimits) $ mkStdGen seed1

finiteHexGrid :: IHexGridCursor Int -> Point2D -> Point2D -> [FiniteRow]
finiteHexGrid ihgCursor (width,height) (x,y) =
  ns ++ [r] ++ ss
  where
    r = finiteHexRow (row ihgCursor) width x
    ns = reverse [ finiteHexRow rnC width x | rnC <- take y $ north ihgCursor ]
    ss = [ finiteHexRow rsC width x | rsC <- take (height - y) $ south ihgCursor ]

finiteHexRow :: IHexRowCursor Int -> Int -> Int -> FiniteRow
finiteHexRow ihrCursor width xpos =
   ( offset' , w ++ [pov'] ++ e )
   where
     offset' = offset ihrCursor
     offsetExtra = if (offset' == Complete) then 0 else 1
     pov' = pov ihrCursor
     w = reverse $ take xpos $ west ihrCursor
     e = take (width - xpos + offsetExtra) $ east ihrCursor
