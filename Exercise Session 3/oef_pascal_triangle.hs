
module Template where

-- * Pascal's Triangle
-- ----------------------------------------------------------------------------

row :: [Integer] -> [Integer]
row xs = zipWith (+) (0:xs) (xs++[0])

pascal :: [[Integer]]
pascal = [1] : map row pascal

bincoeff :: Int -> Int -> Integer
bincoeff n k = (pascal !! n) !! k

-- From the definition:
--   bincoeff n k = product [1..n] `div` (product [1..k] * product [1..n-k])
