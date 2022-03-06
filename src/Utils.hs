module Utils (
    replace
    ,frac
    ,getRGB
    ,grayscale
    ,std
    ,bright
    ,fg16
    ,bg16
    ,fg256
    ,bg256
    ,toNorm
  ) where

frac :: (Integral a) => a -> a -> Double
frac m n = fromIntegral (n `mod` m) / fromIntegral m

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ replace (drop (length find) s) find repl
        else head s : replace (tail s) find repl

-- 8-bit C O L O R S

rgb       = [16..232]   :: [Int]
grayscale = [232..255]  :: [Int]
bright    = [8..16]     :: [Int]
std       = [0..8]      :: [Int]

getRGB :: Int -> Int -> Int -> Int
getRGB r g b =
  rgb !! idx
  where
    idx = 36*r + 6*g + b

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
