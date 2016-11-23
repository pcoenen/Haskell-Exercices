
module Template where

-- * Sequences
-- ----------------------------------------------------------------------------

class Sequence a where
  prev :: a -> a
  next :: a -> a

instance Sequence Int where
  prev x = x - 1 
  next = (+) 1

instance Sequence Char where
  prev = findNext (reverse ['a'..'z'])
  next = findNext ['a'..'z']

findNext :: [Char] -> Char -> Char
findNext (x:y:xs) a
    | x == a = y
    | otherwise = findNext (y:xs) a

instance Sequence Bool where
  prev = not
  next = not

class Sequence a => LeftBoundedSequence a where
  firstElem :: a

class Sequence a => RightBoundedSequence a where
  lastElem :: a

instance LeftBoundedSequence Int where
  firstElem = error "Not implemented"

instance LeftBoundedSequence Char where
  firstElem = error "Not implemented"

instance LeftBoundedSequence Bool where
  firstElem = error "Not implemented"

instance RightBoundedSequence Int where
  lastElem = error "Not implemented"

instance RightBoundedSequence Char where
  lastElem = error "Not implemented"

instance RightBoundedSequence Bool where
  lastElem = error "Not implemented"


