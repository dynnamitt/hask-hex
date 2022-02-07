module Main where

-- test ::: import Characters
import System.Random (mkStdGen, StdGen, getStdGen, randomR)
import Data.Char
import Data.List (zip, transpose)
import Data.Sequence (mapWithIndex)
import Data.Maybe

seed = 2022
screenLen = 80

type Hexcode = Char

{-
    A B
   S * ?
    ? ?
-}

data HexCell = HexCell {
  branchedFrom::Influencer, -- what is the cost?
  branchedTimes::Int,
  xPos::Int,
  hexcode::Hexcode
} deriving (Show)

data Influencer = AboveA HexCell
                | AboveB HexCell
                | Side HexCell
                | NoInflu
                deriving (Show)

-- map :: (a -> b) -> [a] -> [b]
-- map _ [] = []
-- map f (x:xs) = f x : map f xs

hexedCellRow :: Maybe [Char] -> [Char] -> [HexCell]
hexedCellRow aboveRow currRow =
    [HexCell NoInflu 0 p c | c <- currRow,
                             p <- [0..currLen]]
    where
      currLen = length currRow

takeInflu :: StdGen -> Maybe [HexCell] -> Maybe HexCell -> (Influencer,StdGen)
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
      aboveA = aboveRow !! 1 -- ? This is a Maybe also !!!!!!!!!
      aboveB = aboveRow !! 2
takeInflu g (Just aboveRow) (Just prevCell)
    | outcome == 1 = (Side prevCell, g')
    | outcome == 2 = (AboveA aboveA, g')
    | outcome == 3 = (AboveB aboveB, g')
    | otherwise = (NoInflu, g')
    where
      (outcome, g' ) = randomR (1::Int, 4) g
      aboveA = aboveRow !! ((xPos prevCell) + 1 )
      aboveB = aboveRow !! ((xPos prevCell) + 2 )

-- recursive generator
randHexcodes :: StdGen -> [Hexcode]
randHexcodes g =
  let (c, g') = randomR ('A','E') g
  in c : randHexcodes g'

chunkIntoString :: Int -> [Hexcode] -> [String]
chunkIntoString lineW cs =
  let line = [take lineW cs_]
  in line ++ chunkIntoString lineW (drop lineW cs_)
  where cs_ = filter isPrint cs

-- shew with "holes"
hexedRows :: Int -> [String] -> [String]
hexedRows maxCols rows =
  let holes = replicate maxCols ' '
      bipolarRows = zip rows $ cycle [True,False]
  in map (\(row,skew) -> interleaveSkewed row holes skew) bipolarRows
  where
    interleaveSkewed x y True = interleaveLists [x, y]
    interleaveSkewed x y False = interleaveLists [y, x]

interleaveLists :: [[a]] -> [a]
interleaveLists = concat . transpose


main :: IO ()
main = do
  let maxCols = screenLen `div` 2
  let hexcodes = randHexcodes $ mkStdGen seed
  let rowStream = chunkIntoString maxCols hexcodes
  mapM_ putStrLn $ hexedRows maxCols rowStream
