module InfiniteHexGrid(
      initIHexGrid
      ,initIHexRow
      ,finiteHexRow
      ,finiteHexGrid
      ,heroBase
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
data RowOffset = Complete | CappedEnds deriving (Show,Eq,Ord)
heroBase = 1000

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

initIHexGrid :: RandomGen g => g -> (Int,Int)-> IHexGridCursor Int
initIHexGrid g rRange =
  IHexGridCursor n row' s
  where
    s0:s1:northSeeds = unfoldr (Just . uniform) g :: [Int]
    southSeeds = unfoldr (Just . uniform) $ mkStdGen s0 :: [Int]
    row' = initIHexRow (mkStdGen s1) rRange CappedEnds
    offsets = cycle [Complete,CappedEnds]
    n = map (\(s,off) -> initIHexRow (mkStdGen s) rRange off) $ zip northSeeds offsets
    s = map (\(s,off) -> initIHexRow (mkStdGen s) rRange off) $ zip southSeeds offsets


initIHexRow :: RandomGen g => g ->  (Int,Int)-> RowOffset -> IHexRowCursor Int
initIHexRow g1 rRange =
  IHexRowCursor w pov' e
  where
    seed0:eastSeed:_ = unfoldr (Just . uniform) g1 :: [Int]
    (pov', westSeed) = uniformR rRange $ mkStdGen seed0
    w = unfoldr (Just . uniformR rRange) westSeed
    e = unfoldr (Just . uniformR rRange) $ mkStdGen eastSeed

finiteHexGrid ::  Point2D -> Point2D -> IHexGridCursor Int -> [FiniteRow]
finiteHexGrid (width,height) (x,y) ihgCursor =
  ns ++ [r] ++ ss
  where
    r = finiteHexRow heroBase width x (row ihgCursor)
    ns = reverse [ finiteHexRow 0 width x rnC | rnC <- take y $ north ihgCursor ]
    ss = [ finiteHexRow 0 width x rsC | rsC <- take (height - y) $ south ihgCursor ]

finiteHexRow ::  Int -> Int -> Int -> IHexRowCursor Int -> FiniteRow
finiteHexRow b width xpos ihrCursor =
   ( offset' , w ++ [pov'] ++ e )
   where
     offset' = offset ihrCursor
     offsetExtra = if offset' == Complete then 0 else 1
     pov' = pov ihrCursor + b
     w = reverse $ take xpos $ west ihrCursor
     e = take (width - xpos + offsetExtra) $ east ihrCursor
