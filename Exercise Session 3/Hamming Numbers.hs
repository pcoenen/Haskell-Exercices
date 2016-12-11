
module Template where

-- * Hamming Numbers
-- ----------------------------------------------------------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge a b
    | [] <- a = b
    | [] <- b = a
merge (x:xs) (y:ys)
    | x == y    = x : (merge xs ys)
    | x < y     = x : (merge xs (y:ys))
    | otherwise = y : (merge (x:xs) ys)

hamming :: [Integer]
hamming = 1 : map (2*) hamming `merge`
              map (3*) hamming `merge`
              map (5*) hamming


