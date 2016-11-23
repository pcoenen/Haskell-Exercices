
module Template where

-- * Function Chaining
-- ----------------------------------------------------------------------------

applyAll :: [a -> a] -> a -> a
applyAll [] x = x
applyAll (f:fs) x = f $ applyAll fs x

applyTimes :: Int -> (a -> a) -> a -> a
applyTimes am f v
    |am <= 0 = v
    |otherwise = f  $ applyTimes (am-1) f v

applyMultipleFuncs :: a -> [a -> b] -> [b]
applyMultipleFuncs v = foldr (\ f -> (:) (f v)) []


