
module Template where

-- * Exercise 5: Prime Numbers
-- ----------------------------------------------------------------------------

sieve :: Int -> [Int]
sieve m = removeM [2..m]

removeM [] = []
removeM (x:xs) = x : removeM [e | e <- xs, mod e x /= 0]

-- -------------------------
-- Some useful functions
-- -------------------------
sqrtMono :: Double -> Double
sqrtMono = sqrt

i2d :: Int -> Double
i2d = fromIntegral

floorMono :: Double -> Int
floorMono = floor
-- -------------------------

floorSquare :: Int -> Int
floorSquare = floorMono . sqrtMono . i2d

fastSieve :: Int -> [Int]
fastSieve n = fastRemoveM [2..n] n

fastRemoveM [] n = []
fastRemoveM (x:xs) n
    | x > floorSquare n = (x:xs)
    | otherwise = x : fastRemoveM  [e | e <- xs, mod e x /= 0] n


