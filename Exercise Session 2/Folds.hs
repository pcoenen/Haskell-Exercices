
module Template where

-- * Folds
-- ----------------------------------------------------------------------------

mySum :: [Integer] -> Integer
mySum = foldr (+) 0

myProduct :: [Integer] -> Integer
myProduct = foldr (*) 1

foldInts :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
foldInts fn base [] = base

foldInts fn base (x:xs) = fn x (foldInts fn base xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl fn base [] = base
myFoldl fn base (x:xs) = let zq = fn base x
                         in myFoldl fn zq xs  

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr fn base [] = base
myFoldr fn base (x:xs) = fn x (myFoldr fn base xs)  

readInBase :: Int -> [Int] -> Int
readInBase base digits = myFoldl (\x y -> base*x + y) 0 digits

myMap :: (a -> b) -> [a] -> [b]
myMap fn [] = []
myMap fn (x:xs) = fn x : myMap fn xs 

myMapF :: (a -> b) -> [a] -> [b]
myMapF fn = foldr ((:) . fn) []
