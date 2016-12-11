module Template where

import Control.Applicative -- backwards compatibility
import Control.Monad

-- * Maybe Monad
-- ----------------------------------------------------------------------------

sumABC :: [(String, Int)] -> Maybe Int
sumABC list = case lookup "A" list of
                Just va -> case lookup "B" list of
                             Just vb -> case lookup "C" list of
                                          Just vc -> Just (va+vb+vc)
                                          Nothing -> Nothing
                             Nothing -> Nothing
                Nothing -> Nothing

sumABCBind :: [(String, Int)] -> Maybe Int
sumABCBind list = lookup "A" list >>= \va ->
                  lookup "B" list >>= \vb ->
                  lookup "C" list >>= \vc ->
                  return (va+vb+vc)

sumABCDo :: [(String, Int)] -> Maybe Int
sumABCDo list = do
  va <- lookup "A" list
  vb <- lookup "B" list
  vc <- lookup "C" list
  return (va+vb+vc)
