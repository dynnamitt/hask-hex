module InfiniteHexGrid(
      initIHexGrid
      ,move
      ,Direction(..)
      ,RowOffset(..)
      ,IHexGrid(..)
      ,IHexRow(..)
  ) where

import Data.List (unfoldr)
import Utils
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
move dir (IHexGrid n@(n':ns) r s@(s':ss)) =
  case dir of
    West -> IHexGrid (moveRows dir n) (moveRowWest r) (moveRows dir s)
    East -> IHexGrid (moveRows dir n) (moveRowEast r) (moveRows dir s)
    North -> IHexGrid ns n' (r:s)
    South -> IHexGrid (r:n) s' ss

moveRows :: Direction -> [IHexRow a] -> [IHexRow a]
moveRows dir (x:xs) =
  case dir of
    West -> moveRowWest x:moveRows dir xs
    East -> moveRowEast x:moveRows dir xs

moveRowWest :: IHexRow a -> IHexRow a
moveRowWest (IHexRow w@(w':ws) p e@(e':es) o) =
  IHexRow ws w' (p:e) o

moveRowEast :: IHexRow a -> IHexRow a
moveRowEast (IHexRow w@(w':ws) p e@(e':es) o) =
  IHexRow (p:w) e' es o


initIHexGrid :: (RealFrac a,RandomGen g) => g -> (Int,Int)-> IHexGrid a
initIHexGrid g rRange =
  IHexGrid n row' s
  where
    s0:s1:northSeeds = unfoldr (Just . uniform) g :: [Int]
    southSeeds = unfoldr (Just . uniform) $ mkStdGen s0 :: [Int]
    row' = initIHexRow (mkStdGen s1) rRange CappedEnds
    offsets = cycle [Complete,CappedEnds]
    n = map (\(s,off) -> initIHexRow (mkStdGen s) rRange off) $ zip northSeeds offsets
    s = map (\(s,off) -> initIHexRow (mkStdGen s) rRange off) $ zip southSeeds offsets


initIHexRow :: (RealFrac a,RandomGen g) => g ->  (Int,Int) -> RowOffset -> IHexRow a
initIHexRow g1 rRange@(_,hi) =
  IHexRow w (frac hi pov') e
  where
    seed0:eastSeed:_ = unfoldr (Just . uniform) g1 :: [Int]
    (pov', g2) = uniformR rRange $ mkStdGen seed0
    w = map (frac hi) $ unfoldr (Just . uniformR rRange) g2
    e = map (frac hi) $ unfoldr (Just . uniformR rRange) $ mkStdGen eastSeed
