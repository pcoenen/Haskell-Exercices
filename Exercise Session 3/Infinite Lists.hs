
module Template where

-- * Infinite Lists
-- ----------------------------------------------------------------------------

odds :: [Int]
odds = [e | e <- [1..], odd e]

pythagorean :: [(Int, Int, Int)]
pythagorean = [(a,floor . sqrt . fromIntegral $ c*c - a*a,c) | c <- [1..], a <- [1..c-1], (floor . sqrt . fromIntegral $ c*c - a*a) > a, a*a + (floor . sqrt . fromIntegral $ c*c - a*a)^2 == c*c]

partialSums :: Num a => [a] -> [a]
partialSums n = [sum $ take i n | i <- [1..]]

moessner :: Int -> [Int]
moessner n = helper n [1..]

helper 1 a = a
helper n a = helper (n-1) (partialSums [a !! (i-1) |i <- [1..], mod i n /= 0])
