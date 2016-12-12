module MOPL where

-- Exercise 1: Data types for MOPL
data Statement = DEFINE_POSSIBLE_STATEMENTS

data Term = DEFINE_POSSIBLE_TERMS

-- Exercise 2: Helper functions for MOPL
assign :: String -> Term -> Statement
assign = error "Not implemented"

printTerm :: Term -> Statement
printTerm = error "Not implemented"

intTerm :: Int -> Term
intTerm = error "Not implemented"

varTerm :: String -> Term
varTerm = error "Not implemented"

plus :: Term -> Term -> Term
plus = error "Not implemented"

times :: Term -> Term -> Term
times = error "Not implemented"

minus :: Term -> Term -> Term
minus = error "Not implemented"

-- Exercise 3: The state of a MOPL program
type State = [(String,Int)]

valueOf :: State -> String -> Int
valueOf = error "Not implemented"

insertS :: String -> Int -> State -> State
insertS = error "Not implemented"

-- Exercise 4: The evaluation of a term
evalTerm :: State -> Term -> Int
evalTerm = error "Not implemented"

-- Exercise 5: The execution of an assignment
execAssign :: String -> Term -> State -> State
execAssign = error "Not implemented"

-- Exercise 6: The execution of a sequence of statements
execPure :: State -> [Statement] -> State
execPure = error "Not implemented"

-- Exercise 7: The full language
execute :: [Statement] -> IO ()
execute = error "Not implemented"


