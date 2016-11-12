module Ex3FunctionsCodeGenerator where
import Ex3FunctionsTypes

regsNotInUse = [D1,D2,D3,D4,D5,D6,D7]

-- GENERATE CODE FOR FUNCTIONS. RETURN IN D1
transFunction :: Function -> [Instr]

-- Functions return in D0
-- So the last thing that the function does have to be put in
transFunction (Defun fname paramname body)
 = [Define fname] ++ (transExp body regsNotInUse) ++ [Ret]

-- IDENTIFY REGISTERS IN USE, PUSH THEM ON STACK
saveRegs :: [Register] -> [Register]
saveRegs regsNotInUse
 =  []

-- RESTORES REGISTERS FROM THE STACK
--restoreRegs regsNotInUse
-- = []

-- GENERATE FUNCTION FOR EXPRESSIONS
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
transExp (Minus e1 e2) (dst:next:rest)
 = (transExp e2 (next:rest)) ++
   (transExp e1 (dst:rest)) ++
   [Sub (Reg next) (Reg dst)]

transExp (Plus e1 e2) (dst:next:rest)
 = (transExp e2 (next:rest)) ++
   (transExp e1 (dst:rest)) ++
   [Add (Reg next) (Reg dst)]

-- MOVE THE VARIABLE YOU'RE PASSING IN 
-- PASSED IN GOES INTO D0
-- FUNCTION RETURNS ARGUMENT IN D1
transExp (Apply string exp) (dst:rest)
  = [Mov (Reg dst) (Reg D0)] ++ [Jsr string]

weight :: Exp -> Int

weight (Const x) = 1
weight (Var x)   = 1
weight (Minus e1 e2)
 = min cost1 cost2
 where
  cost1 = max (weight e1) ((weight e2) + 1)
  cost2 = max ((weight e1) + 1) (weight e2)
