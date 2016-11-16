module Template where

-- * Exercise 5: Prime Numbers
-- ----------------------------------------------------------------------------

sieve :: Int -> [Int]

sieve m = deleteMul [2..m]

deleteMul :: [Int] -> [Int]

deleteMul [] = []

deleteMul (x:xs) = x : (deleteMul (deleteMultiplicates x xs))

deleteMultiplicates :: Int -> [Int] -> [Int]

deleteMultiplicates x [] = []
    
deleteMultiplicates x (y:ys)
    | mod y x == 0 = deleteMultiplicates x ys
    | otherwise = y : (deleteMultiplicates x ys)

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
floorSquare n = error "Not implemented"

fastSieve :: Int -> [Int]
fastSieve n = error "Not implemented"

