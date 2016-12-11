
module Template where

import Control.Applicative -- backwards compatibility
import Control.Monad

-- * Maybe Monad
-- ----------------------------------------------------------------------------

sumABC :: [(String, Int)] -> Maybe Int
sumABC list = case lookup "A" list of
                Just a  -> case lookup "B" list of
                            Just b  -> case lookup "C" list of
                                        Just c  -> Just (a+b+c)
                                        Nothing -> Nothing
                            Nothing -> Nothing
                Nothing -> Nothing

sumABCBind :: [(String, Int)] -> Maybe Int
sumABCBind list = 
    lookup "A" list >>= \a -> 
    lookup "B" list >>= \b -> 
    lookup "C" list >>= \c -> 
    Just (a+b+c)

sumABCDo :: [(String, Int)] -> Maybe Int
sumABCDo list = do  
    a <- lookup "A" list
    b <- lookup "B" list
    c <- lookup "C" list
    return (a+b+c)


