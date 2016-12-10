sumf :: [Float] -> Float
sumf [] = 0
sumf (x:xs) = x + sumf xs

productf :: [Float] -> Float
productf [] = 1
productf (x:xs) = x * product xs

piSum :: Float -> Float
piSum n = 8 * sumf [1/((4*q +1)*(4*q+3))| q <- [0..n] ]

piProd :: Float -> Float
piProd n = 4* productf [((2*q+2)*(2*q+4))/((2*q+3)**2) | q <- [0..n]]
