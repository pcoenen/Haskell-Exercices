module Template where

-- * Arithmetic Expressions
-- ----------------------------------------------------------------------------

data Exp = Const Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
  deriving (Show, Eq)

eval :: Exp -> Int

eval (Const x) = x

eval (Add x y) = eval x + eval y

eval (Sub x y) = (eval x) - (eval y)

eval (Mul x y) = eval x * eval y    

data Inst = IPush Int | IAdd | ISub | IMul
  deriving (Show, Eq)

type Prog  = [Inst]
type Stack = [Int]

runtimeError :: Stack
runtimeError = error "Runtime error."

execute :: Inst -> Stack -> Stack

execute x (een:twee:rest)
    | x == IAdd = ((een + twee):rest)
    | x == ISub = ((twee - een):rest)
    | x == IMul = ((een * twee):rest)

execute (IPush x) (rest) = x : rest

run :: Prog -> Stack -> Stack

run [] stack = stack

run (x:xs) stack = run xs (execute x stack)

compile :: Exp -> Prog

compile x
    | Add one two <- x = (compile one) ++ (compile two) ++ [IAdd]
    | Sub one two <- x = (compile one) ++ (compile two) ++ [ISub]
    | Mul one two <- x = (compile one) ++ (compile two) ++ [IMul]
    | Const y <- x = [IPush y]
