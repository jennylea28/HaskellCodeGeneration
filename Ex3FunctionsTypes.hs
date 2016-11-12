-- Compilers Exercise 3:

-- This module defines a simple expressions/functions language as a
-- Haskell data type, together with a data type for 68K-like instructions
-- for represening the output of the code generator.

-- Paul Kelly  Imperial College London  2006

-- Changelog:
-- Feb 2009 parameterised by parameter and result register ids
-- Feb 2006 Created PHJK
-- Feb 2006 Added "instance show Instr" thanks to Dave Ingram

module Ex3FunctionsTypes where

type Prog = [Function]
data Function = Defun String String Exp

--                deriving Show
instance Show Function where
  show (Defun n p e) = "\n" ++ n ++ "(" ++ p ++ ") { return " ++ show e ++ "; }\n"
  showList [] = showString ""
  showList (f:fs) = shows f . showl fs
      where showl [] = showString ""
            showl (f:fs) = shows f . showl fs
data Exp = Const Int | Var String | Plus Exp Exp | 
           Minus Exp Exp | Apply String Exp
--           deriving Show
instance Show Exp where
  show (Const i) = show i
  show (Var s)   = s
  show (Plus e1 e2) = show e1 ++ " + " ++ show e2
  show (Minus e1 e2) = show e1 ++ " - " ++ show e2
  show (Apply s e) = s ++ "(" ++ show e ++ ")"

-- 68000 instruction set as a Haskell data type

data Instr = Define String       -- "label:"
           | Jsr String          -- jump to subroutine, push PC
           | Ret                 -- return from subroutine, pop PC from stack
           | Mov Operand Operand -- "mov.l xxx yyy" (yyy:=xxx)
           | Sub Operand Operand -- "sub.l xxx yyy" (yyy:=yyy-xxx)
           -- additional instructions and directives not actually
           -- needed for exercise 3:
           | Add Operand Operand -- "add.l xxx yyy" (yyy:=yyy+xxx)
           | Cmp Operand Operand 
           | Mul Operand Operand
           | Div Operand Operand
           | Bra [Char] 
           | Blt [Char] 
           | Bgt [Char] 
           | Ble [Char] 
           | Bge [Char] 
           | Halt
           | Comm String Int     -- ".comm name size"
instance Show Instr where
  show (Define s) = s ++ ": "
  show (Jsr s) = "\n\tjsr  " ++ s
  show (Ret) = "\n\tret"
  show (Mov o Push) = "\n\tpush " ++ show o
  show (Mov Pop o) = "\n\tpop  " ++ show o
  show (Mov o1 o2) = "\n\tmov  " ++ show o1 ++ ", " ++ show o2
  show (Sub o1 o2) = "\n\tsub  " ++ show o1 ++ ", " ++ show o2
  show (Add o1 o2) = "\n\tadd  " ++ show o1 ++ ", " ++ show o2
  show (Mul o1 o2) = "\nmul  " ++ show o1 ++ ", " ++ show o2
  show (Div o1 o2) = "\ndiv  " ++ show o1 ++ ", " ++ show o2
  show (Cmp o1 o2) = "\ncmp  " ++ show o1 ++ ", " ++ show o2
  show (Bra s) = "\tbra  " ++ s
  show (Blt s) = "\tblt  " ++ s
  show (Bgt s) = "\tbgt  " ++ s
  show (Bge s) = "\tbge  " ++ s
  show (Halt)  = "\thalt "
  show (Comm name size) = ".comm\t" ++ show name ++ ", " ++ show size

data Operand = Reg Register -- specifies data or address register
             | Push         -- "-(a7)" (as in "mov.w d0,-(a7)" to push d0)
             | Pop          -- "(a7)+" (so "Mov Pop (Reg D0)" = mov.w (a7)+,d0)
             | ImmNum Int   -- "#n"
             -- not needed for Exercise 3:
             | ImmName String -- to access value of symbolic address defined using .comm
             | Abs String    -- to access location addressed by symbolic address
             | Ind Register  -- register indirect
--     deriving Show
instance Show Operand where
  show (Reg r) = show r
  show (ImmNum i) = "$" ++ show i

data Register = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | A7
     deriving (Eq, Show)

paramReg = D0
resultReg = D1
allRegs = [D0, D1,D2,D3,D4,D5,D6,D7]

-- Test examples:

-- test5 
--  = [Defun "dec" "x" (Minus (Var "x") (Const 1)), Defun "main" "x" (Minus (Const 2) (Apply "dec" (Minus (Const 3)(Var "x"))))]

-- test6
--  = [Defun "fib" "x" (Plus (Apply "fib" (Minus (Var "x")(Const 1)))
--                           (Apply "fib" (Minus (Var "x")(Const 2))))]