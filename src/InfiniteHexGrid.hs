module InfiniteHexGrid(
      initIHexGrid
      ,IHexGridCursor(..)
      ,IHexRowCursor(..)
  ) where

  import System.Random (StdGen, newStdGen, getStdGen, randomR, randomRs)

  data IHexRowCursor a = IHexRowCursor {
    west :: [a],
    pov :: a,
    east :: [a],
    skewed :: Bool
  } deriving (Show, Eq, Ord)

  data IHexGridCursor a = IHexGridCursor {
    north :: [IHexRowCursor a],
    row :: IHexRowCursor a,
    south :: [IHexRowCursor a]
  } deriving (Show, Eq, Ord)

  randLimits = (0,9)

  type GenTriple = (StdGen,StdGen,StdGen)

  initIHexGrid :: IO (IHexGridCursor Int)
  initIHexGrid = do
    gt1:gt2:xs <- sequence [genTriplet,genTriplet,genTriplet]
    let gt3 = head xs
    let row' = initIHexRow gt1 True
    let n = initIHexRow gt2 <$> cycle [False,True]
    let s = initIHexRow gt3 <$> cycle [False,True]
    return $ IHexGridCursor n row' s

  genTriplet :: IO GenTriple
  genTriplet = do
    x:y:z <- sequence $ take 3 $ cycle [getStdGen,newStdGen]
    return (x,y,head z)

  initIHexRow :: GenTriple -> Bool -> IHexRowCursor Int
  initIHexRow (g1,g2,g3) skew =
    IHexRowCursor w pov' e skew
    where
      (pov',_) = randomR randLimits g1
      w = randomRs randLimits g2
      e = randomRs randLimits g3
