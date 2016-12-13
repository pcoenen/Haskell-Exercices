
module Template where

-- * Exercise 1: Lists
-- ----------------------------------------------------------------------------

myLast :: [a] -> a
myLast [] = error "out of range"
myLast [x] = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast [_] = error "out of range"
myButLast [] = error "out of range"
myButLast [x,y] = x
myButLast (x:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "out of range"
elementAt (x:xs) 1  = x
elementAt (x:xs) y = elementAt xs (y-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = myReverse x == x

compress :: Eq a => [a] -> [a]
compress (x:y:l) 
    | x == y    = compress (x:l)
    | otherwise = x : compress (y:l)
compress y = y

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = helper [x] xs

helper :: Eq a => [a] -> [a] -> [[a]]
helper x [] = [x]
helper (y:ys) (x:xs)
    | x == y    = helper (y:y:ys) xs
    | otherwise = (:) (y:ys) (helper [x] xs)

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = helperEncode (1,x) xs

helperEncode :: Eq a => (Int, a) -> [a] -> [(Int,a)]
helperEncode x [] = [x]
helperEncode (n,y) (x:xs)
    | x == y    = helperEncode (n+1,y) xs
    | otherwise = (:) (n,y) (helperEncode (1,x) xs)


