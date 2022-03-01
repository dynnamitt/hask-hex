{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Colors
import Text.Printf
import Data.Maybe
import Data.Char
import qualified System.Console.Terminal.Size as TS

main :: IO ()
main = do
  win <- TS.size
  let (TS.Window _ w)  = fromJust win :: TS.Window Int
  let cols = div w 16
  mapM_ putStrLn $ toRows cols charCodeTbl

toRows :: Int -> [String] -> [String]
toRows cols tbl
  | length tbl > cols  = row' cols <> (toRows cols $ drop cols tbl)
  | otherwise           = row' $ length tbl
  where
    row' x = map (<>"\t") $ take x tbl


charCodeTbl :: [String]
charCodeTbl =
  [ showN i <> " " <> bg16 False 1 <> "   " <> showC i  <> toNorm | i <- [1..1300]]

showN :: Int -> String
showN x =
  replicate (4 - length x') '0' ++ x'
  where
    x' = show x

showC :: Int -> String
showC x
  | isPrint x' = x':[]
  | otherwise = showLitChar x' ""
  where
    x' = chr x
