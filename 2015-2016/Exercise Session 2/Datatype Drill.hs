
module Template where

import Data.List

-- Section 1.1: Data type definitions
-- ----------------------------------------------------------------------------

data IntTree = EmptyIntTree
             | IntTreeNode Int IntTree IntTree
             deriving (Show)
e1 :: IntTree
e1 = IntTreeNode 1 (IntTreeNode 2 EmptyIntTree EmptyIntTree) (IntTreeNode 3 EmptyIntTree EmptyIntTree)


data Pos -- A type representing positions.. No need to tinker with it

data ChessPiece = CP String (Pos -> Pos)

data Station a b = M [(a,Int)] b | L [(a,Int)] b (Station a b)

-- Section 1.2: Functions over the new types
-- ----------------------------------------------------------------------------

mapIntTree :: (Int -> Int) -> IntTree -> IntTree
mapIntTree f t 
    | t == EmptyIntTree = EmptyIntTree
    | IntTreeNode x l r <- t = IntTreeNode (f x) (mapIntTree f l) (mapIntTree f r)

intTree2list :: IntTree -> [Int] -- preorder
intTree2list EmptyIntTree = []
intTree2list (IntTreeNode x l r) = (:) x $ (intTree2list l) ++ (intTree2list r)

instance Eq IntTree where
  (==) EmptyIntTree EmptyIntTree = True
  (==) (IntTreeNode x l1 r1) (IntTreeNode y l2 r2) = (((==) True (l1 == l2)) == (r1 == r2)) == (x == y)
  (==) _ _ = False

data Tree a = EmptyTree
            | TreeNode a (Tree a) (Tree a)
            deriving (Show)

e3 = TreeNode 4 (TreeNode 5 EmptyTree EmptyTree) (TreeNode 6 EmptyTree EmptyTree)
e2 = TreeNode 1 (TreeNode 2 e3 EmptyTree) (TreeNode 3 (TreeNode 7 EmptyTree EmptyTree) (TreeNode 8 EmptyTree EmptyTree))

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f t 
    | EmptyTree <- t = EmptyTree
    | TreeNode x l r <- t = TreeNode (f x) (mapTree f l) (mapTree f r)

instance Functor Tree where
  fmap = mapTree

tree2list :: Tree a -> [a] -- preorder
tree2list EmptyTree = []
tree2list (TreeNode x l r) = (:) x $ (tree2list l) ++ (tree2list r)

foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree f b t = foldr f b (tree2list t)

instance Eq a => Eq (Tree a) where
  (==) EmptyTree EmptyTree = True
  (==) t1 t2 = equals (tree2list t1) (tree2list t2)

equals :: Eq a => [a] -> [a] -> Bool
equals x y 
    | x \\ y == y \\ x = True
    | otherwise = False

tree2listBF :: Tree a -> [a] -- breadth-first
tree2listBF t = helper [t]

helper :: [Tree a] -> [a]
helper [] = []
helper ((TreeNode x l r):xs) = x : (helper $ xs ++ [l] ++ [r])
helper (EmptyTree:xs) = helper xs

machine :: [(a, Int)] -> b -> Station a b
machine = M

combine :: [Station a b] -> Station a b
combine [x] = x
combine (x:xs) 
    | M x y <- x = L x y (combine xs)
    | L x y s <- x = L x y (combine xs)
