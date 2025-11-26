module Ast where

data Expr = Plus Expr Expr | Minus Expr Expr | Times Expr Expr | Div Expr Expr
    | Literal Float

eval :: Expr -> Float
-- Your code goes here
eval (Literal x) = x
eval (Plus e1 e2) = eval e1 + eval e2
eval (Minus e1 e2) = eval e1 - eval e2
eval (Times e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 / eval e2

eq :: Expr -> Expr -> Bool
-- Your code goes here
eq (Literal x) (Literal y) = x == y
eq (Plus x y)      (Plus a b)      = eq x a && eq y b
eq (Minus x y)     (Minus a b)     = eq x a && eq y b
eq (Times x y)     (Times a b)     = eq x a && eq y b
eq (Div x y)       (Div a b)       = eq x a && eq y b
eq _               _               = False

-- Should eval to "5.0"
test1 = Plus (Literal 3.0) (Literal 2.0)

-- Should eval to "3.5"
test2 = Plus (Literal 3.0) (Div (Literal 1.0) (Literal 2.0))

-- Should eval to "15.5"
test3 = Plus (Times (Literal 3.0) (Literal 5.0)) (Div (Literal 1.0) (Literal 2.0))

