
module Template where

-- * Exercise 2: Lists, Continued
-- ----------------------------------------------------------------------------

decode :: [(Int, a)] -> [a]
decode = concat . map generate

generate :: (Int, a) -> [a]
generate (0,_) = []
generate (n,x) = x : (generate (n-1,x))

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : (dupli xs)

repli :: [a] -> Int -> [a]
repli l n = concat $ map (copyChar n) l

copyChar :: Int -> a -> [a]
copyChar 0 a = []
copyChar n a = a : (copyChar (n-1) a)

dropEvery :: [a] -> Int -> [a]
dropEvery = helperDrop 1

helperDrop :: Int -> [a] -> Int -> [a]
helperDrop _ [] _ = []
helperDrop h (x:xs) n
    | h == n = helperDrop 1 xs n
    | otherwise = x  : (helperDrop (h+1) xs n)

split :: [a] -> Int -> ([a], [a])
split x n 
    |length x < n   = error "out of range"
    |otherwise      = (take n x, drop n x)

slice :: [a] -> Int -> Int -> [a]
slice x i j
    | j > length x  = error "out of range" 
    | i < 0         = error "out of range" 
    | i > j         = []
    | otherwise     = take (j-i+1) (drop (i-1) x)

rotate :: [a] -> Int -> [a]
rotate x n = drop (mod n (length x)) x ++ take (mod n (length x)) x

removeAt :: Int -> [a] -> (a, [a])
removeAt n x
    | n > length x  = error "out of range" 
    | otherwise     = (x !! (n-1), remove n x)

remove :: Int -> [a] -> [a]
remove _ [] = error "out of range" 
remove n (x:xs)
    | n == 1    = xs
    | otherwise = x : remove (n-1) xs 
