
module Ex3FunctionsTests where

test0 = [Defun "main" "x" (Minus (Var "x")(Const 300))]

test1 = [Defun "dec" "x" (Minus (Var "x") (Const 1)), Defun "main" "x" (Minus (Const 2) (Apply "dec" (Minus (Const 1)(Var "x"))))]

test2 = [Defun "dec" "x" (Minus (Var "x") (Const 1)), Defun "main" "x" (Minus (Apply "dec" (Var "x")) (Const 100))]

test3 = [Defun "basea" "x" (Minus (Var "x") (Const 1)), Defun "baseb" "x" (Minus (Var "x") (Const 100)), Defun "f5" "x" (Minus (Apply "basea" (Var "x")) (Apply "baseb" (Var "x"))), Defun "f4" "x" (Minus (Apply "basea" (Var "x")) (Apply "f5" (Var "x"))), Defun "f3" "x" (Minus (Apply "f5" (Var "x")) (Apply "f4" (Var "x"))), Defun "f2" "x" (Minus (Apply "f4" (Var "x")) (Apply "f3" (Var "x"))), Defun "f1" "x" (Minus (Apply "f3" (Var "x")) (Apply "f2" (Var "x"))), Defun "main" "x" (Apply "f1" (Var "x"))]

test4 = [Defun "dec" "x" (Minus (Var "x") (Const 1)), Defun "main" "x" (Minus (Apply "dec" (Var "x")) (Apply "dec" (Const 1000)))]

test5 = [Defun "dec" "x" (Minus (Var "x") (Const 1)), Defun "main" "x" (Minus (Const 2) (Apply "dec" (Minus (Const 3)(Var "x"))))]

test6 = [Defun "fib" "x" (Plus (Apply "fib" (Minus (Var "x")(Const 1))) (Apply "fib" (Minus (Var "x")(Const 2))))]