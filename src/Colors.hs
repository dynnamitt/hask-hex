module Colors(fg16,bg16,fg256,bg256,toNorm )  where


fg256 :: Int -> String
fg256 num = "\ESC[38;5;" ++ show num ++ "m"

bg256 :: Int -> String
bg256 num = "\ESC[48;5;" ++ show num ++ "m"

fg16 :: Bool -> Int -> String
fg16 hi = nixEscSafe hi . (+30)

bg16 :: Bool -> Int -> String
bg16 hi = nixEscSafe hi . (+40)

toNorm :: String
toNorm = nixEscSafe False 0

nixEscSafe :: Bool -> Int -> String
nixEscSafe False num = "\ESC[" ++ show num ++ "m"
nixEscSafe True num = "\ESC[" ++ show num ++ ";1m"
