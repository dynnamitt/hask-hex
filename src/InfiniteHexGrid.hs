module InfiniteHexGrid(
      initIHexGrid
      ,showHexRow
      ,showHexGrid
      ,FiniteRow
      ,IHexGridCursor(..)
      ,IHexRowCursor(..)
  ) where

import System.Random (StdGen, newStdGen, getStdGen, randomR, randomRs)

type Point2D = (Int, Int)
type Box2D = (Point2D, Point2D)
type GenTriple = (StdGen,StdGen,StdGen)
type FiniteRow = (Bool,[Int])
randLimits = (0,9)

data IHexRowCursor a = IHexRowCursor {
  west :: [a],
  pov :: a,
  east :: [a],
  skewed :: Bool
} deriving (Show, Eq, Ord)

showHexRow :: IHexRowCursor Int -> Int -> Int -> FiniteRow
showHexRow rcursor width xpos =
   ( skewed rcursor , w' ++ [pov'] ++ e' )
   where
     pov' = pov rcursor
     w' = reverse $ take xpos $ west rcursor
     e' = take (width - xpos) $ east rcursor

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


initIHexGrid :: IO (IHexGridCursor Int)
initIHexGrid = do
  gt1:gt2:xs <- sequence [genTriplet,genTriplet,genTriplet]
  let gt3 = head xs
  let row' = initIHexRow gt1 (Just 99) True
  let n = initIHexRow gt2 Nothing <$> cycle [False,True]
  let s = initIHexRow gt3 Nothing <$> cycle [False,True]
  return $ IHexGridCursor n row' s

genTriplet :: IO GenTriple
genTriplet = do
  x:y:z <- sequence $ take 3 $ cycle [getStdGen,newStdGen]
  return (x,y,head z)

initIHexRow :: GenTriple -> Maybe Int -> Bool  -> IHexRowCursor Int
initIHexRow (g1,g2,g3) povOverride skew =
  IHexRowCursor w (getPov povOverride) e skew
  where
    getPov Nothing = pov'
    getPov (Just n) = n
    (pov',_) = randomR randLimits g1
    w = randomRs randLimits g2
    e = randomRs randLimits g3
