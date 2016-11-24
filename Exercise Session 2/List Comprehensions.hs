
module Template where

-- * List Comprehensions
-- ----------------------------------------------------------------------------

mapLC :: (a -> b) -> [a] -> [b]
mapLC fn list = [fn x | x <- list]

filterLC :: (a -> Bool) -> [a] -> [a]
filterLC pr list = [a | a <- list, pr a]


