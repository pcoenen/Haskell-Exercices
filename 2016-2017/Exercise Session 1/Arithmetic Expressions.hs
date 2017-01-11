
module ArithmeticExpressions where

-- * Arithmetic Expressions
-- ----------------------------------------------------------------------------

data Exp = Const Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
  deriving (Show, Eq)

eval :: Exp -> Int
eval e
    | Const x <- e = x
    | Add x y <- e = (eval x) + (eval y)
    | Sub x y <- e = (eval x) - (eval y)
    | Mul x y <- e = (eval x) * (eval y)

data Inst = IPush Int | IAdd | ISub | IMul
  deriving (Show, Eq)

type Prog  = [Inst]
type Stack = [Int]

runtimeError :: Stack
runtimeError = error "Runtime error."

execute :: Inst -> Stack -> Stack
execute (IPush x) s = x:s
execute i (f:s:r)
    |IAdd  <-   i = (f + s) : r
    |ISub <-    i = (s - f) : r
    |IMul <-    i = (f * s) : r

run :: Prog -> Stack -> Stack
run [] s = s 
run (x:xs) s = run xs (execute x s) 

compile :: Exp -> Prog
compile  e
    | Const x <- e = [IPush x]
    | Add x y <- e = (compile x) ++ (compile y) ++ [IAdd]
    | Sub x y <- e = (compile x) ++ (compile y) ++ [ISub]
    | Mul x y <- e = (compile x) ++ (compile y) ++ [IMul]

