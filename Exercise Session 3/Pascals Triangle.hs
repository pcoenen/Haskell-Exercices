
module Template where

-- * Pascal's Triangle
-- ----------------------------------------------------------------------------

row :: [Integer] -> [Integer]
row x = [1] ++ [(x !! i) + (x !! (i+1))| i <- [0..(length x - 2)]]  ++ [1]

pascal :: [[Integer]]
pascal = [1] : [row x | x <- pascal]

bincoeff :: Int -> Int -> Integer
bincoeff n k = (pascal !! n) !! k


