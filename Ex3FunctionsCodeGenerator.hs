module Ex3FunctionsCodeGenerator where
import Ex3FunctionsTypes

import Data.List

regsToUse = allRegs \\ [paramReg]

-- FUNCTIONS RETURN IN D1
-- PUT LAST THING IN D1
transFunction :: Function -> [Instr]

transFunction (Defun fname paramname body)
 = [Define fname] ++ (transExp body regsToUse) ++ [Ret]

-- PUSH REGISTERS ONTO THE STACK
saveRegs :: [Register] -> [Instr]
saveRegs regsAvailable
 = map (\x-> Mov (Reg x) Push) unused
 where
   unused = allRegs \\ regsAvailable

-- POP REGISTERS FROM THE STACK
restoreRegs :: [Register] -> [Instr]
restoreRegs regsAvailable
 = map (\x-> Mov Pop (Reg x)) used
 where
   used = reverse $ allRegs \\ regsAvailable


-- LEAVE RESULT IN FIRST REGISTER IN LIST
-- PARAMETER VAR X STORED IN REGISTER D0
transExp :: Exp -> [Register] -> [Instr]

transExp (Const x) (dest:rest)
 = [Mov (ImmNum x) (Reg dest)]

transExp (Var x) (dest:rest)
 = [Mov (Reg paramReg) (Reg dest)]

-- SUBTRACT E1 FROM E2
-- PUT E2 RESULT IN D2
-- PUT E1 RESULT IN D1
-- SUB D1 FROM D0
transExp (Minus e1 e2) (dest:next:rest)
 = if weight e1 > weight e2 then
  (transExp e1 (dest:next:rest)) ++
  (transExp e2 (next:rest)) ++
  [Sub (Reg next) (Reg dest)]
  else
  (transExp e2 (next:dest:rest)) ++
  (transExp e1 (dest:rest)) ++
  [Sub (Reg next) (Reg dest)]

-- MOVE THE VARIABLE YOU'RE PASSING IN 
-- PASSED IN GOES INTO D0
-- FUNCTION RETURNS ARGUMENT IN D1
transExp (Apply string e) regs@(dest:next:rest)
  = (saveRegs regs) ++ 
    (transExp e regs) ++
    [Mov (Reg dest) (Reg paramReg)] ++ 
    [Jsr string] ++ 
    [Mov (Reg paramReg) (Reg dest)] ++ 
    (restoreRegs regs)

weight :: Exp -> Int
weight (Const x) = 1
weight (Var x)   = 1
weight (Minus e1 e2)
 = min cost1 cost2
 where
  cost1 = max (weight e1) ((weight e2) + 1)
  cost2 = max ((weight e1) + 1) (weight e2)
weight (Apply string e) = 1 + (weight e)

