{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Colors
import Text.Printf
import Data.Maybe
import Data.Char
import Data.List
import qualified System.Console.Terminal.Size as TS

hi = 3000
spaceOccupied = 17

main :: IO ()
main = do
  win <- TS.size
  let (TS.Window _ w)  = fromJust win :: TS.Window Int
  let cols = div w spaceOccupied
  mapM_ putStrLn $ toRows cols charCodeTbl

toRows :: Int -> [String] -> [String]
toRows cols tbl
  | length tbl > cols  = row' cols : (toRows cols $ drop cols tbl)
  | otherwise           = [row' $ length tbl]
  where
    row' x = intercalate "\t" $ take x tbl

charCodeTbl :: [String]
charCodeTbl =
  [ showN i <> " " <> bg16 False 1 <> "   " <> showChar i  <> toNorm
          | i <- [1..hi], valid i]
  where
    showChar = fst . showC
    valid i = not $ generalCategory (chr i) `elem` noUse
    noUse = [NonSpacingMark,SpacingCombiningMark,Control,NotAssigned]

showN :: Int -> String
showN x =
  replicate (4 - length x') '0' ++ x'
  where
    x' = show x

showC :: Int -> (String,GeneralCategory)
showC x
  | isPrint x' = (x':"  " ,generalCategory x')
  | otherwise  = (showLitChar x' "" , generalCategory x')
  where
    x' = chr x
