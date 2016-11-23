
module Template where

-- * List Comprehensions
-- ----------------------------------------------------------------------------

mapLC :: (a -> b) -> [a] -> [b]
mapLC f = foldr ((:) . f) []

filterLC :: (a -> Bool) -> [a] -> [a]
filterLC f [] = []
filterLC f (x:xs)
    | f x = x : filterLC f xs
    | otherwise = filterLC f xs


