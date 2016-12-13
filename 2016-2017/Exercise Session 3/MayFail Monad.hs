module Template where

import Control.Applicative -- backwards compatibility
import Control.Monad

-- * MayFail Monad
-- ----------------------------------------------------------------------------

data MayFail e a = Error e | Result a
  deriving (Eq, Ord, Show)

safeDiv :: Int -> Int -> MayFail String Int
safeDiv a b | b == 0    = Error "Division by zero"
            | otherwise = Result (div a b)

instance Functor (MayFail e) where
  fmap f x 
    | Error e   <- x = Error e
    | Result a  <- x = Result (f a)

instance Applicative (MayFail e) where
  pure  = return
  (<*>) = ap

instance Monad (MayFail e) where
  return a = Result a
  m >>= f = case m of
                Result x -> f x
                Error e -> Error e

data Exp = Lit Int | Add Exp Exp | Mul Exp Exp | Div Exp Exp
  deriving (Eq, Ord, Show)

eval :: Exp -> MayFail String Int
eval (Lit x)    =   return x
eval (Add x y)  =   eval x >>= \a ->
                    eval y >>= \b ->
                    return (a + b)
eval (Mul x y)  =   eval x >>= \a ->
                    eval y >>= \b ->
                    return (a*b)
eval (Div x y)  =   eval x >>= \a ->
                    eval y >>= \b ->
                    safeDiv a b
                    


evalDo :: Exp -> MayFail String Int
evalDo (Lit x)   = return x
evalDo (Add x y) = do 
    a <- eval x
    b <- eval y
    return (a+b)
evalDo (Mul x y) = do
    a <- eval x
    b <- eval y
    return (a*b)
evalDp (Div x y) = do
    a <- eval x
    b <- eval y
    safeDiv a b
                         


