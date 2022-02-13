module InfiniteHexGrid(
      initIHexGrid
  ) where

  import System.Random (StdGen, randomR, randomRs)

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

  initIHexGrid :: GenTriple -> GenTriple -> GenTriple -> IHexGridCursor Int
  initIHexGrid gs1 gs2 gs3 =
    IHexGridCursor n row' s
    where
      row' = initIHexRow gs1 True
      n = initIHexRow gs2 <$> cycle [False,True]
      s = initIHexRow gs3 <$> cycle [False,True]

  initIHexRow :: GenTriple -> Bool -> IHexRowCursor Int
  initIHexRow (g1,g2,g3) skew =
    IHexRowCursor w pov' e skew
    where
      (pov',_) = randomR randLimits g1
      w = randomRs randLimits g2
      e = randomRs randLimits g3
