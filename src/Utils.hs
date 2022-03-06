module Utils (
    replace
    ,frac
    ,fg16
    ,bg16
    ,fg256
    ,bg256
    ,toNorm
  ) where

frac :: (Integral a, Fractional b) => a -> a -> b
frac m n = fromIntegral (n `mod` m) / fromIntegral m

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ replace (drop (length find) s) find repl
        else head s : replace (tail s) find repl

fg256 :: (Integral a, Show a) => a -> String
fg256 num = "\ESC[38;5;" ++ show num ++ "m"

bg256 :: (Integral a, Show a) => a -> String
bg256 num = "\ESC[48;5;" ++ show num ++ "m"

fg16 :: (Integral a, Show a) => Bool -> a -> String
fg16 hi = nixEscSafe hi . (+30)

bg16 :: (Integral a, Show a) => Bool -> a -> String
bg16 hi = nixEscSafe hi . (+40)

toNorm :: String
toNorm = nixEscSafe False 0

nixEscSafe :: (Integral a, Show a) => Bool -> a -> String
nixEscSafe False num = "\ESC[" ++ show num ++ "m"
nixEscSafe True num = "\ESC[" ++ show num ++ ";1m"
