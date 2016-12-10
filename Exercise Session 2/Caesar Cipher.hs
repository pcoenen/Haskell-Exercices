
module Template where

import Data.Char
import Data.List

-- * Caesar Cipher
-- ----------------------------------------------------------------------------

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n ' ' = ' '
shift n c = int2let (mod ((+n) . let2int $ c) 26)

encode :: Int -> String -> String
encode n x = map (shift n) x

table :: [Float]
table = [ 8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4
        , 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1 ]

percent :: Int -> Int -> Float
percent x y = (fromIntegral x) / (fromIntegral y) * 100

amount :: Char -> String -> Int
amount x [] = 0
amount x (y:ys)
    | x == y = 1 + (amount x ys)
    | otherwise = amount x ys

freqs :: String -> [Float]
freqs s = [percent (amount l s) (length s) | l <- ['a'..'z']]

chisqr :: [Float] -> [Float] -> Float
chisqr o e = sum [((oi - ei)**2) / ei| (oi,ei) <- zip o e]

rotate :: Int -> [a] -> [a]
rotate n l = (drop (length l - n) l) ++ (take (length l - n) l)

crack :: String -> String
crack s = s --[ chisqr (rotate n (freqs s)) table | n <- [0..25] ]


