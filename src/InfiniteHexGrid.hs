module InfiniteHexGrid(
      initIHexGrid
      ,move
      ,Direction(..)
      ,RowOffset(..)
      ,IHexGrid(..)
      ,IHexRow(..)
  ) where

import Data.List (unfoldr)
import System.Random (
  RandomGen, uniform, uniformR, mkStdGen)

data RowOffset = Complete | CappedEnds deriving (Show,Eq,Ord)
data Direction = West | East | North | South deriving (Show,Eq)

-- Make monoid, we need an empty/identity for fold?
data IHexRow a = IHexRow {
  west :: [a],
  pov :: a,
  east :: [a],
  offset :: RowOffset
} deriving (Show, Eq, Ord)

data IHexGrid a = IHexGrid {
  north :: [IHexRow a],
  row :: IHexRow a,
  south :: [IHexRow a]
} deriving (Show, Eq, Ord)

move :: Direction -> IHexGrid a -> IHexGrid a
move dir (IHexGrid n@(n':ns) r@(IHexRow w@(w':ws) p e@(e':es) o) s@(s':ss)) =
  case dir of
    West -> IHexGrid n (IHexRow ws w' (p:e) o) s
    East -> IHexGrid n (IHexRow (p:w) e' es o) s
    North -> IHexGrid ns n' (r:s)
    South -> IHexGrid (r:n) s' ss



initIHexGrid :: RandomGen g => g -> (Int,Int)-> IHexGrid Int
initIHexGrid g rRange =
  IHexGrid n row' s
  where
    s0:s1:northSeeds = unfoldr (Just . uniform) g :: [Int]
    southSeeds = unfoldr (Just . uniform) $ mkStdGen s0 :: [Int]
    row' = initIHexRow (mkStdGen s1) rRange CappedEnds
    offsets = cycle [Complete,CappedEnds]
    n = map (\(s,off) -> initIHexRow (mkStdGen s) rRange off) $ zip northSeeds offsets
    s = map (\(s,off) -> initIHexRow (mkStdGen s) rRange off) $ zip southSeeds offsets


initIHexRow :: RandomGen g => g ->  (Int,Int)-> RowOffset -> IHexRow Int
initIHexRow g1 rRange =
  IHexRow w pov' e
  where
    seed0:eastSeed:_ = unfoldr (Just . uniform) g1 :: [Int]
    (pov', g2) = uniformR rRange $ mkStdGen seed0
    w = unfoldr (Just . uniformR rRange) g2
    e = unfoldr (Just . uniformR rRange) $ mkStdGen eastSeed
