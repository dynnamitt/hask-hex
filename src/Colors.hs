module Colors(fgColor, tColor, nixEsc)  where

-- background + HI fg
tColor :: Int -> String
tColor n =
  let bg = 40
      fg = 30
  in
    nixEsc ( bg + n ) ++ nixEsc 1 ++ nixEsc ( fg + n )

fgColor :: Int -> String
fgColor c =
  let fg = 30
  in
    nixEsc $ fg + c

nixEsc :: Int ->String
nixEsc n = "\ESC[" ++ show n ++ "m"
