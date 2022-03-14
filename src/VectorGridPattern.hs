module VectorGridPattern(
  twoDimNoise
  ,fracZoom
  ,cubic
  ,smooth
  ) where

import InfiniteHexGrid
import Utils
import qualified Data.Vector as V

type VGrid a = V.Vector (V.Vector a)
type Transformation a b = (a,a) -> (b,b) -> VGrid a -> a

-- works >1
fracZoom :: RealFrac a => Int -> (Transformation a Int) -> VGrid a -> VGrid a
fracZoom zoom transFn grid =
  V.map (\y -> V.map (\x -> transform (y,x)) cellEnum ) rowEnum
  where
    transform (y,x) = transFn (yZoom y,xZoom x) (yLen,xLen) grid
    rowEnum = V.enumFromTo 0 yLen
    cellEnum = V.enumFromTo 0 xLen
    yLen = V.length grid - 1
    xLen = V.length (V.head grid) - 1
    xZoom = fracDiv $ fromIntegral zoom
    yZoom = fracDiv $ fromIntegral $ zoom `div` 2
    fracDiv z = (/z) . fromIntegral

cubic :: RealFrac a => Transformation a Int
cubic (y,x) _ grid =
  grid V.! floor y V.! floor x

-- https://lodev.org/cgtutor/randomnoise.html#Smooth_Noise_
smooth :: RealFrac a => Transformation a Int
smooth (y,x) (h,w) grid =
  (fractX         * fractY        * itm y1 x1) +
  ((1 - fractX)   * fractY        * itm y1 x1) +
  (fractX         * (1 - fractY)  * itm y2 x1) +
  ((1 - fractX)   * (1 - fractY)  * itm y2 x2)
  where
    -- get fractional part of x and y
    fractX = (x-).fromIntegral.floor $ x
    fractY = (y-).fromIntegral.floor $ y
   -- wrap around
    x1 = (floor x + w) `mod` w
    y1 = (floor y + h) `mod` h
   -- neighbor values
    x2 = (x1 + w - 1) `mod` w
    y2 = (y1 + h - 1) `mod` h
    itm y x = grid V.! y V.! x

twoDimNoise :: (Int,Int) -> (Int,Int) -> IHexGrid a -> VGrid a
twoDimNoise (w,h) (x,y) ihg =
  V.take h $ V.fromList $ ns ++ r ++ ss'
  where
    r = [oneDimNoise (w,x) $ row ihg | y <= 0]
    ss' = if y > 0 then drop (y-1) ss else ss
    ss = [ oneDimNoise (w,x) s | s <- south ihg ]
    ns = reverse . take (-y) $ [ oneDimNoise (w,x) n | n <- north ihg ]

oneDimNoise :: (Int,Int) -> IHexRow a -> V.Vector a
oneDimNoise (len,xpos) ihr =
  V.take len $ V.fromList $ w ++ pov' ++ e
  where
    pov' = [pov ihr| xpos <= 0]
    e = if xpos > 0
        then drop (xpos-1) $ east ihr
        else east ihr
    w = reverse . take (-xpos) $ west ihr
