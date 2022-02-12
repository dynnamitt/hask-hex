module Colors(fgC, c0, wrapC )  where


wrapC :: String -> Int -> String
wrapC s n = fgC n ++ s ++ c0

fgC :: Int -> String
fgC c =
  let foundation = 30
  in
    nixEsc 1 ++ nixEsc ( foundation + c )

c0 :: String
c0 = nixEsc 0

nixEsc :: Int -> String
nixEsc n = "\ESC[" ++ show n ++ "m"
