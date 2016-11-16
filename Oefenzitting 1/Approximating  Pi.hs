module Template where

-- * Approximating Pi
-- ----------------------------------------------------------------------------

sumf :: [Float] -> Float

sumf [] = 0

sumf (x:xs) = x + sumf xs

productf :: [Float] -> Float

productf [] = 1

productf (x:xs) = x * productf xs

piSum :: Float -> Float

piSum n = 8 * (sumf [(1/((4*n+1)*(4*n + 3))) | n <- [0..n]])

piProd :: Float -> Float

piProd n = 4 * (productf [((2*n+2)*(2*n+4)/(2*n+3)^2) | n <- [0..n]])


