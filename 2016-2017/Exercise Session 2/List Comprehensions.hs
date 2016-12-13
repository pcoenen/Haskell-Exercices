
module Template where

-- * List Comprehensions
-- ----------------------------------------------------------------------------

mapLC :: (a -> b) -> [a] -> [b]
mapLC f list = [f x | x <- list]

filterLC :: (a -> Bool) -> [a] -> [a]
filterLC f list = [x | x <- list, f x == True]


