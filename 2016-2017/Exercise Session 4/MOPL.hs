module MOPL where
--Exercise 1
data Statement = Ass String Term 
    |Print Term

data Term = V String
    |I Int
    |B (Int -> Int -> Int) Term Term

--Exercise 2
assign :: String -> Term -> Statement
assign = Ass 

printTerm :: Term -> Statement
printTerm = Print

intTerm :: Int -> Term
intTerm = I

varTerm :: String -> Term 
varTerm = V

plus :: Term -> Term -> Term
plus = B (+)

times :: Term -> Term -> Term
times = B (*)

minus :: Term -> Term -> Term
minus = B (-)

--Exercise 3
type State = [(String,Int)]

valueOf :: State -> String -> Int
valueOf ((k,v):xs) x 
    | k == x = v
    | otherwise = valueOf xs x

insertS :: String -> Int -> State -> State
insertS nk nv [] = [(nk,nv)]
insertS nk nv ((k,v):xs)
    | nk == k = ((k,nv):xs)
    | otherwise = (k,v) : (insertS nk nv xs)

--Exercise 4
evalTerm :: State -> Term -> Int 
evalTerm s t 
    | I x <- t = x
    | V x <- t = valueOf s x
    | B f x y <- t = f (evalTerm s x) (evalTerm s y)

--Exercise 5
execAssign :: String -> Term -> State -> State
execAssign v t s = insertS v (evalTerm s t) s 

--Exercise 6
type Program = [Statement]

execPure :: State -> Program -> State
execPure s [] = s
execPure s (p:ps)
    | Print x <- p = execPure s ps
    | Ass v t <- p = execPure (execAssign v t s) ps

--Exercise 7

execute :: Program -> IO ()
execute = execute2 []
execute2 s [] = return ()
execute2 s (p:ps)
    | Print x <- p = do
        print $ evalTerm s x
        execute2 s ps
    | Ass v t <- p = execute2 (execAssign v t s) ps
