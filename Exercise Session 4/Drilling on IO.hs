module Template where

import Control.Applicative -- backwards compatibility
import Control.Monad

-- * Functors
-- ----------------------------------------------------------------------------

data Identity a = Identity a
  deriving (Eq, Ord, Show)

data Pair a b = Pair a b
  deriving (Eq, Ord, Show)

data Unit a = Unit

instance Eq (Unit a) where
  Unit == Unit = True

instance Show (Unit a) where
  show Unit = "Unit"

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Functor Unit where
  fmap _ Unit = Unit


main = 
    -- first
    second

readInt :: IO Int
readInt = do
    x <- readLn
    return x

first = do
    m <- readInt
    n <- readInt
    mapM_ print [n | i <- [1..m]]

second = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn . reverse $ line  
            main

index :: [IO a] -> IO Int -> IO a
index list i = do 
    x <- i
    (list) !! x
