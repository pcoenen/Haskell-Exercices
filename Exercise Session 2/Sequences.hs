module Template where

-- * Sequences
-- ----------------------------------------------------------------------------

class Sequence a where
  prev :: a -> a
  next :: a -> a

instance Sequence Int where
  prev x = x - 1
  next x = x + 1

instance Sequence Char where
  prev x = nextChar (reverse ['a'..x]) x
  next x = nextChar [x .. 'z'] x

nextChar :: [Char] -> Char -> Char

nextChar (y:ys) x
    | x == y = head ys
    | otherwise = nextChar ys x

instance Sequence Bool where
  prev = not
  next = not

class Sequence a => LeftBoundedSequence a where
  firstElem :: a

class Sequence a => RightBoundedSequence a where
  lastElem :: a

instance LeftBoundedSequence Int where
  firstElem = minBound :: Int

instance LeftBoundedSequence Char where
  firstElem = 'a'

instance LeftBoundedSequence Bool where
  firstElem = False

instance RightBoundedSequence Int where
  lastElem = maxBound :: Int

instance RightBoundedSequence Char where
  lastElem = 'z'

instance RightBoundedSequence Bool where
  lastElem = True


