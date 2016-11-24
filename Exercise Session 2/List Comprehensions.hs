
module Template where

-- * List Comprehensions
-- ----------------------------------------------------------------------------
mapLC :: (a -> b) -> [a] -> [b]
mapLC f l = [f n | n <- l]

filterLC :: (a -> Bool) -> [a] -> [a]
filterLC f l = [n | n <- l, f n]    


