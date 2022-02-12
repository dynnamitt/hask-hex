module Influence ( Hexcode
                  ,HexCell(..)
                  ,Influencer(..)
                  ,takeInflu
                  ) where

import System.Random (mkStdGen, StdGen, getStdGen, randomR, randomRs)

type Hexcode = Int

{-
    A B
   S * ?
    ? ?
-}

data Influencer = AboveA HexCell
                | AboveB HexCell
                | Side HexCell
                | NoInflu
                deriving (Show,Eq)

-- not important , just to show `comprehende
instance Ord Influencer where
  AboveA hc1 `compare` AboveA hc2 = hc1 `compare` hc2
  AboveA _ `compare` AboveB _ = GT
  AboveA _ `compare` Side _ = GT
  AboveA _ `compare` NoInflu = LT -- ? kind of sense
  AboveB hc1 `compare` AboveB hc2 = hc1 `compare` hc2
  AboveB _ `compare` Side _ = GT
  AboveB _ `compare` NoInflu = LT -- ? kind of sense
  Side hc1 `compare` Side hc2 = hc1 `compare` hc2
  Side _ `compare` NoInflu = LT -- ? kind of sense
  NoInflu `compare` NoInflu = EQ

data HexCell = HexCell {
  branchedFrom::Influencer, -- what is the cost?
  branchedTimes::Int,
  rowNum::Int,
  cellNum::Int,
  hexcode::Hexcode
} deriving (Show,Eq)

-- not important , just to show `comprehende
instance Ord HexCell where
  (HexCell _ _ r1 c1 _) `compare` (HexCell _ _ r2 c2 _) = (r1,c1) `compare` (r2,c2)


world3x3 = replicate 3 [0..2]

-- generateLines :: StdGen -> [[HexCell]]
-- generateLines g =
--   foldr (\line acc -> acc ++ [innerF g acc line] ) [] world3x3
--   where
--     innerF g [] [] = foldr (\i acc -> takeInflu g Nothing Nothing) []
--     innerF g [] (x:xs) = foldr (\i acc-> takeInflu g Nothing (Just x)) []
--     innerF g (y:ys) (x:xs) = foldr (\i acc-> takeInflu g (Just y) (Just x)) []


takeInflu :: StdGen -> Maybe [HexCell] -> Maybe HexCell -> (Influencer ,StdGen)
takeInflu g Nothing Nothing = (NoInflu, g)
takeInflu g Nothing (Just prevCell)
    | outcome <= 70 = (Side prevCell, g')
    | otherwise = (NoInflu, g')
    where
      (outcome, g' ) = randomR (1::Int, 100) g
takeInflu g (Just (x:y:xs)) Nothing
    | outcome == 1 = (AboveA aboveA, g')
    | outcome == 2 = (AboveB aboveB, g')
    | otherwise = (NoInflu, g')
    where
      (outcome, g' ) = randomR (1::Int, 3) g
      aboveA = x -- ? This is a Maybe also !!!!!!!!!
      aboveB = y
takeInflu g (Just aboveRow) (Just prevCell)
    | outcome == 1 = (Side prevCell, g')
    | outcome == 2 = (AboveA aboveA, g')
    | outcome == 3 = (AboveB aboveB, g')
    | otherwise = (NoInflu, g')
    where
      (outcome, g' ) = randomR (1::Int, 4) g
      aboveA = aboveRow !! (cellNum prevCell + 1 )
      aboveB = aboveRow !! (cellNum prevCell + 2 )
