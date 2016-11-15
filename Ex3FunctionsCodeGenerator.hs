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
   unused = regsToUse \\ regsAvailable

-- POP REGISTERS FROM THE STACK
restoreRegs :: [Register] -> [Instr]
restoreRegs regsAvailable
 = map (\x-> Mov Pop (Reg x)) used
 where
   used = reverse $ regsToUse \\ regsAvailable


-- LEAVE RESULT IN FIRST REGISTER IN LIST
-- PARAMETER VAR X STORED IN REGISTER D0
transExp :: Exp -> [Register] -> [Instr]

transExp (Const x) (dst:rest)
 = [Mov (ImmNum x)(Reg dst)]

transExp (Var x) (dst:rest)
 = [Mov (Reg paramReg)(Reg dst)]

-- SUBTRACT E1 FROM E2
-- PUT E2 RESULT IN D2
-- PUT E1 RESULT IN D1
-- SUB D1 FROM D0
transExp (Minus e1 e2) regs@(dst:next:rest)
 = if weight e1 > weight e2 then
  (transExp e1 regs) ++
  (transExp e2 (next:rest)) ++
  [Sub (Reg dst) (Reg next)]
  else
  (transExp e2 (next:dst:rest)) ++
  (transExp e1 (dst:rest)) ++
  [Sub (Reg dst) (Reg next)]

transExp (Plus e1 e2) (dst:next:rest)
 = (transExp e2 (next:rest)) ++
   (transExp e1 (dst:rest)) ++
   [Add (Reg next) (Reg dst)]

-- MOVE THE VARIABLE YOU'RE PASSING IN 
-- PASSED IN GOES INTO D0
-- FUNCTION RETURNS ARGUMENT IN D1
transExp (Apply string e) regs@(dst:rest)
  = (saveRegs regs) ++ 
    (transExp e regsToUse) ++
    [Mov (Reg dst) (Reg paramReg)] ++ 
    [Jsr string] ++ 
    [Mov (Reg resultReg) (Reg D0)] ++ 
    (restoreRegs regs)

weight :: Exp -> Int
weight (Const x) = 1
weight (Var x)   = 1
weight (Minus e1 e2)
 = min cost1 cost2
 where
  cost1 = max (weight e1) ((weight e2) + 1)
  cost2 = max ((weight e1) + 1) (weight e2)
weight (Apply string e) = 1

