myAnd :: [Bool] -> Bool

myAnd [] = True

myAnd(x:xs)
    | x == True = myAnd(xs)
    | x == False = False

myOr :: [Bool] -> Bool

myOr [] = False

myOr(x:xs)
    | x == True = True
    | x == False = myOr(xs)

append :: [Int] -> [Int] -> [Int]

append x y = x ++ y

myProduct :: [Integer] -> Integer

myProduct (x:[]) = x

myProduct (x:xs) = myProduct(xs) * x

insert :: Int -> [Int] -> [Int]

insert x (y:ys)
    | x <= y = x:y:ys
    | otherwise = insert2 x ys [y]

insert2 x (y:ys) z
    | x <= y = z ++ (x:y:ys)
    | otherwise = insert2 x ys (z ++ [y])

myLast :: [Int] -> Int

myLast (x:[]) = x

myLast (x:xs) = myLast xs
