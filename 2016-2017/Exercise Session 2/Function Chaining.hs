
module Template where

-- * Function Chaining
-- ----------------------------------------------------------------------------

applyAll :: [a -> a] -> a -> a
applyAll [] x = x
applyAll (f:fs) x = f . (applyAll fs) $ x

applyTimes :: Int -> (a -> a) -> a -> a
applyTimes 0 f v = v
applyTimes x f v = f . (applyTimes (x-1) f) $ v

applyMultipleFuncs :: a -> [a -> b] -> [b]
applyMultipleFuncs v [] = []
applyMultipleFuncs v (f:fs) = (f v) : (applyMultipleFuncs v fs)
