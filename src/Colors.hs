module Colors(fgC,bgC,toNorm )  where

bg = 40
fg = 30


fgC :: Int -> String
fgC = nixEsc . (+fg)

bgC :: Int -> String
bgC = nixEsc . (+bg)

toNorm :: String
toNorm = nixEsc 0

nixEsc :: Int -> String
nixEsc n = "\ESC[" ++ show n ++ "m"
