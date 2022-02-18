module InfiniteHexGrid(
      initIHexGrid
      --,initIHexRow
      ,RowOffset(..)
      ,IHexGridCursor(..)
      ,IHexRowCursor(..)
  ) where

import Data.List (unfoldr)
import System.Random (
  RandomGen, uniform, uniformR, mkStdGen)


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
    (pov', g2) = uniformR rRange $ mkStdGen seed0
    w = unfoldr (Just . uniformR rRange) g2
    e = unfoldr (Just . uniformR rRange) $ mkStdGen eastSeed
