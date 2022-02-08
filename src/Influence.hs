module Influence where


import System.Random (mkStdGen, StdGen, getStdGen, randomR)


{-
    A B
   S * ?
    ? ?
-}

data HexCell = HexCell {
  branchedFrom::Influencer HexCell, -- what is the cost?
  branchedTimes::Int,
  xPos::Int,
  hexcode::Hexcode
} deriving (Show)

data Influencer a = AboveA a
                | AboveB a
                | Side a
                | NoInflu
                deriving (Show)

-- map :: (a -> b) -> [a] -> [b]
-- map _ [] = []
-- map f (x:xs) = f x : map f xs
firstCell = HexCell NoInflu 0 0

hexedCellRow :: StdGen -> Maybe [HexCell] -> [Char] -> [HexCell]
hexedCellRow g Nothing currRow =
    [cell p c | c <- currRow, p <- [0..currLen]]
    where
      currLen = length currRow
      cell 0 c = firstCell c
      -- cell n c = stuck
      --   let (influ,g') = takeInflu g Nothing


takeInflu :: StdGen -> Maybe [HexCell] -> Maybe HexCell -> (Influencer HexCell,StdGen)
takeInflu g Nothing Nothing = (NoInflu, g)
takeInflu g Nothing (Just prevCell)
    | outcome <= 70 = (Side prevCell, g')
    | otherwise = (NoInflu, g')
    where
      (outcome, g' ) = randomR (1::Int, 100) g
takeInflu g (Just aboveRow) Nothing
    | outcome == 1 = (AboveA aboveA, g')
    | outcome == 2 = (AboveB aboveB, g')
    | otherwise = (NoInflu, g')
    where
      (outcome, g' ) = randomR (1::Int, 3) g
      aboveA = aboveRow !! 0 -- ? This is a Maybe also !!!!!!!!!
      aboveB = aboveRow !! 1
takeInflu g (Just aboveRow) (Just prevCell)
    | outcome == 1 = (Side prevCell, g')
    | outcome == 2 = (AboveA aboveA, g')
    | outcome == 3 = (AboveB aboveB, g')
    | otherwise = (NoInflu, g')
    where
      (outcome, g' ) = randomR (1::Int, 4) g
      aboveA = aboveRow !! ((xPos prevCell) + 1 )
      aboveB = aboveRow !! ((xPos prevCell) + 2 )
