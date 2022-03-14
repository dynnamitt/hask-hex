
module VectorGridPattern(
  oneDimNoise
  ,twoDimNoise
  ,fracZoom
  ) where

import InfiniteHexGrid
import qualified Data.Vector as V

type VGrid a = V.Vector (V.Vector a)

-- works >1
fracZoom :: Int -> VGrid a -> VGrid a
fracZoom zoom grid =
  V.map (\y -> V.map (\x -> cubic (yZoom y, xZoom x) grid) cellEnum ) rowEnum
  where
    rowEnum = V.enumFromTo 0 $ V.length grid - 1
    cellEnum = V.enumFromTo 0 $ V.length (V.head grid) - 1
    xZoom = fracDiv $ fromIntegral zoom
    yZoom = fracDiv $ fromIntegral $ zoom `div` 2
    fracDiv z = (/z) . fromIntegral

cubic :: RealFrac b => (b,b) -> VGrid a -> a
cubic (y,x) xs =
  xs V.! floor y V.! floor x

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
