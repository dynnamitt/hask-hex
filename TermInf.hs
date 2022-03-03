{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Colors
import Text.Printf
import Data.Maybe
import Data.Char
import Data.List
import qualified System.Console.Terminal.Size as TS
import System.Environment

-- https://jrgraphix.net/research/unicode_blocks.php
ranges = [

    -- COL1
    ([0x0020..0x007F],"Basic Latin")
    ,([0x00A0..0x00FF],"Latin-1 Supplement")
    ,([0x0100..0x017F],"Latin Extended-A")
    ,([0x0180..0x024F],"Latin Extended-B")
    ,([0x0250..0x02AF],"IPA Extensions")
    ,([0x02B0..0x02FF],"Spacing Modifier Letters")
    --,([0x0300..0x036F],"Combining Diacritical Marks")
    ,([0x0370..0x03FF],"Greek+Coptic")
    ,([0x0590..0x05FF],"Hebrew")
    ,([0x16A0..0x16FF],"Runic")
    -- COL 2
    ,([0x2580..0x259F],"Block Elems")
    ,([0x25A0..0x25FF],"Geometric Shapes")
    ,([0x2600..0x26FF],"Miscellaneous Symbols")
    ,([0x2700..0x27BF],"Dingbats")

  ]
spaceOccupied = 12

main :: IO ()
main = do
  win <- TS.size
  -- [lo,hi] <- getArgs
  -- let lo' = read lo
  -- let hi' = read hi
  let (TS.Window _ w)  = fromJust win :: TS.Window Int
  let cols = div w spaceOccupied
  mapM_ (block cols) ranges

block :: Int -> ([Int],String) -> IO ()
block cols (xs,name) = do
  putStrLn $ (fg16 True 3) <> name <> toNorm
  mapM_ putStrLn $ toRows cols $ charCodeTbl xs

toRows :: Int -> [String] -> [String]
toRows cols tbl
  | length tbl > cols  = row' cols : (toRows cols $ drop cols tbl)
  | otherwise           = [row' $ length tbl]
  where
    row' x = intercalate "  " $ take x tbl

charCodeTbl :: [Int] -> [String]
charCodeTbl xs =
  [ showN i <> " " <> bg16 True 4 <> " " <> showChar i  <> toNorm
          | i <- xs]
  where
    showChar = fst . showC

showN :: Int -> String
showN x =
  replicate (5 - length x') '·' ++ x'
  where
    x' = show x

showC :: Int -> (String,GeneralCategory)
showC x
  | isPrint x' = (x':"  " ,generalCategory x')
  | otherwise  = ("_?_" , generalCategory x')
  where
    x' = chr x
