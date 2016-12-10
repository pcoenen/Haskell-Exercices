
module Template where

-- * List Comprehensions Without The Sugar
-- ----------------------------------------------------------------------------

lc1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
lc1 f p as = [f a | a <- as, p a]

lc2 :: (a -> b -> c) -> [a] -> (a -> [b]) -> (b -> Bool) -> [c]
lc2 f as bf p = [f a b | a <- as, b <- bf a, p b]

lc3 :: (Int -> Int -> Int -> a) -> Int -> [a]
lc3 f n = [ f a b c | a <- [1..n]
          , b <- [a..n], even a
          , c <- [b..n]
          , a * a + b * b == c * c
          ]

lc1' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
lc1' f p as = map f (filter p as)

lc2' :: (a -> b -> c) -> [a] -> (a -> [b]) -> (b -> Bool) -> [c]
lc2' f as bf p = concat (map (\a -> map (f a) (concat (map (filter p) (map bf [a])))) as)

lc3' :: (Int -> Int -> Int -> a) -> Int -> [a]
lc3' f n = concat $ map (\ a -> concat $ map (\ b -> map (f a b) (filter (\ c -> a * a + b * b == c * c) [b..n])) [a..n]) (filter even [1..n])


