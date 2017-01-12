
module Template where

-- * Hamming Numbers
-- ----------------------------------------------------------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge l [] = l
merge [] l = l
merge (x:xs) (y:ys)
    | x < y     = x : (merge xs (y:ys))
    | x == y    = x : (merge xs ys)
    | otherwise = y : (merge (x:xs) ys)

hamming :: [Integer]
hamming = 1 : merge2
    where
        list1 = [2 * e | e <- hamming]
        list2 = [3 * e | e <- hamming]
        list3 = [5 * e | e <- hamming]
        merge1 = merge list1 list2
        merge2 = merge merge1 list3
