module Ex3FunctionsCodeGenerator where
import Ex3FunctionsTypes

import Data.List

regsToUse = allRegs \\ [paramReg]

-- FUNCTIONS RETURN IN D1
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

-- PARAMETER VAR X STORED IN REGISTER D0
transExp :: Exp -> [Register] -> [Instr]

transExp (Const x) (dest:_)
 = [Mov (ImmNum x) (Reg dest)]

transExp (Var x) (dest:_)
 = [Mov (Reg paramReg) (Reg dest)]

-- SUBTRACT E1 FROM E2
transExp (Minus e1 e2) (dest:next:rest)
 = if weight e1 > weight e2 then
  (transExp e1 (dest:next:rest)) ++
  (transExp e2 (next:rest)) ++
  [Sub (Reg next) (Reg dest)]
  else
  (transExp e2 (next:dest:rest)) ++
  (transExp e1 (dest:rest)) ++
  [Sub (Reg next) (Reg dest)]

-- If we are calling a function with x
-- We dont need to put it into D1 (already there)
transExp (Apply string (Var _)) regs@(dest:rest)
  = (saveRegs regs) ++ 
    [Jsr string] ++ 
    (if resultReg == dest then [] else
    [Mov (Reg resultReg) (Reg dest)]) ++ 
    (restoreRegs regs)

-- PASSED IN D0, RetURN IN D1
transExp (Apply string e) regs@(dest:rest)
  = (saveRegs regs) ++ 
    (transExp e regs) ++
    (if dest == paramReg then [] else 
    [Mov (Reg dest) (Reg paramReg)]) ++ 
    [Jsr string] ++ 
    (if resultReg == dest then [] else
    [Mov (Reg resultReg) (Reg dest)]) ++ 
    (restoreRegs regs)


weight :: Exp -> Int
weight (Const _) = 1
weight (Var _)   = 1
weight (Minus e1 e2)
 = min cost1 cost2
 where
  cost1 = max (weight e1) ((weight e2) + 1)
  cost2 = max ((weight e1) + 1) (weight e2)
weight (Apply _ e) = 1 + (weight e)

