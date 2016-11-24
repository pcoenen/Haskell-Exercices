
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
myFoldl fn base (x:xs) = fn (myFoldl fn base xs) x  

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr fn base [] = base
myFoldr fn base (x:xs) = fn x (myFoldr fn base xs)  

readInBase :: Int -> [Int] -> Int
readInBase base digits = 
  myFoldl horner 0 (reverse digits)
  where horner x y = y + (x * base)

myMap :: (a -> b) -> [a] -> [b]
myMap fn [] = []
myMap fn (x:xs) = fn x : myMap fn xs 

myMapF :: (a -> b) -> [a] -> [b]
myMapF fn = foldr ((:) . fn) []
