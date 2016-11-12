--Ex3FunctionsEmulator.lhs

-------------------------------------------------------------
--Interpreter/Virtual machine for a 68k-style instruction set
--Instructions are represented as the Haskell data type in Ex3FunctionsTypes.lhs

--7 data registers, 1 address register
--data and address register indirect addressing

--To use it type
--  interpret [Sub (ImmNum 100)(Reg D0)] 1000
--The output is the machine state, showing the result, 900, in register D0.

--Ashley Brown and Paul Kelly, Department of Computing, Imperial College 2002-6
--ashley@ashleybrown.co.uk, p.kelly@imperial.ac.uk

--Change log: 
--2002 written by Ashley
--2006/02 Paul added code to handle jsr/ret.  Also changed register
--  representation for compatibility with exercise specification.

--Dysfeatures:
--Needs to be fixed to work step by step so you can debug.
--Doesn't detect when stack overflows into variables.
--Confused over whether byte or word addressed (particularly wrt jsr/ret)
-------------------------------------------------------------

--Changelog:
--2009-02-06: parameterised by parameter and result register ids

module Ex3FunctionsEmulator where

import Ex3FunctionsTypes

--            registers, a7, mem, symbols, compare
type State = ([Int], Int, [Int], [Symbol], Int)

--             name, size, address
type Symbol = (String, Int, Int)

initialState :: Int -> State
initialState argValue 
  = (initialRegs, memSize-1, allocMem [] memSize, [], 0)
    where
    initialRegs = setVal zeroRegs (regToRegNumber paramReg) argValue
    zeroRegs = [0 | r <- allRegs]
    memSize = 128


--The interpreter: give it the code, and an initial value for register 0

interpret instrs argValue = exec instrs instrs (initialState argValue)

exec :: [Instr] -> [Instr] -> State -> State

exec prog [] state = state
exec prog (Halt:is) state = state

exec prog (Comm name size:is) state = 
  let
    (dn, a7, mem, syms, cmp) = state
    syms' = ((name, size `div` 4, length mem):syms)
    mem' = allocMem mem (size `div` 4)
    state' = (dn, a7, mem', syms', cmp)
  in
    exec prog is state'


exec prog (Cmp op1 op2:is) state = 
  let
    (op1v, state') = getOpVal op1 state
    (op2v, state'') = getOpVal op2 state'
    (dn, a7, mem, syms, cmp) = state''
    cmp' = op1v-op2v
    state''' = (dn, a7, mem, syms, cmp')
  in
    exec prog is state'''

exec prog (Mov op1 op2:is) state = 
  let
    (op1v, state') = getOpVal op1 state
    state'' = setOpVal op2 op1v state'
  in
    exec prog is state''

exec prog (Add op1 op2:is) state = 
  let
    (op1v, state') = getOpVal op1 state
    (op2v, state'') = getOpVal op2 state'
    state''' = setOpVal op2 (op2v+op1v) state''
  in
    exec prog is state'''

exec prog (Mul op1 op2:is) state = 
  let
    (op1v, state') = getOpVal op1 state
    (op2v, state'') = getOpVal op2 state'
    state''' = setOpVal op2 (op2v*op1v) state''
  in
    exec prog is state'''

exec prog (Sub op1 op2:is) state = 
  let
    (op1v, state') = getOpVal op1 state
    (op2v, state'') = getOpVal op2 state'
    state''' = setOpVal op2 (op2v-op1v) state''
  in
    exec prog is state'''

exec prog (Div op1 op2:is) state = 
  let
    (op1v, state') = getOpVal op1 state
    (op2v, state'') = getOpVal op2 state'
    state''' = setOpVal op2 (op2v `div` op1v) state''
  in
    exec prog is state'''

exec prog (Bra name:is) state = jmp prog name state

exec prog (Blt name:is) state =
  let
    (dn, a7, mem, syms, cmp) = state
  in
    if cmp < 0 then
      jmp prog name state
    else
      exec prog is state

exec prog (Ble name:is) state =
  let
    (dn, a7, mem, syms, cmp) = state
  in
    if cmp <= 0 then
      jmp prog name state
    else
      exec prog is state


exec prog (Bgt name:is) state =
  let
    (dn, a7, mem, syms, cmp) = state
  in
    if cmp 0 then
      jmp prog name state
    else
      exec prog is state

exec prog (Bge name:is) state =
  let
    (dn, a7, mem, syms, cmp) = state
  in
    if cmp  0 then
      jmp prog name state
    else
      exec prog is state

exec prog (Jsr name:is) state =
  let
    (dn, a7, mem, syms, cmp) = state
    state' = push pc state
  in
    jmp prog name state'
  where
    pc = getCurrentPC prog is
    push pc (dn, a7, mem, syms, cmp) = 
      let mem' = setVal mem a7 pc
      in 
        if a7-4  0 then 
          (dn, a7-4, mem', syms, cmp)
        else
          error "Stack Overflow in jsr"
    getCurrentPC prog is = (length prog) - (length is)

exec prog (Ret:is) state =
  let
    (dn, a7, mem, syms, cmp) = state
    (newPC, state') = pop state
  in
    jmpToAddress prog newPC state'
  where
    pop (dn, a7, mem, syms, cmp)
      = (mem !! (a7+4),
         (dn, a7+4, mem, syms, cmp))
    jmpToAddress prog newPC state = 
      exec prog is state
      where
        (skipped, is) = splitAt newPC prog

exec prog (i:is) state = exec prog is state

-----------------------------------------------------

jmp prog name state = ijmp prog prog name state
  where
    ijmp prog (Define x:is) name state
      | x == name = exec prog is state
      | otherwise = ijmp prog is name state
    
    ijmp prog (i:is) name state = ijmp prog is name state

-----------------------------------------------------

getOpVal :: Operand -> State -> (Int, State)
getOpVal (ImmNum x) state = (x, state)

getOpVal (ImmName x) state = (getSymAdd x syms, state)
  where
    (dn, a7, mem, syms, cmp) = state

getOpVal (Reg r) state = (getReg r state, state)
  where
    (dn, a7, mem, syms, cmp) = state
    
getOpVal (Reg A7) state = (a7, state)
  where
    (dn, a7, mem, syms, cmp) = state
    
getOpVal (Abs name) state = (val, state)
  where
    (dn, a7, mem, syms, cmp) = state
    add = getSymAdd name syms
    val = mem !! (add `div` 4)

getOpVal (Ind r) state = (val, state)
  where
    (dn, a7, mem, syms, cmp) = state
    add = getReg r state
    val = mem !! (add `div` 4)

getOpVal (Ind A7) state = (val, state)
  where
    (dn, a7, mem, syms, cmp) = state
    add = a7
    val = mem !! (add `div` 4)

getOpVal Pop state = (val, (dn, a7', mem, syms, cmp))
  where
    (dn, a7, mem, syms, cmp) = state
    a7' = a7+4
    add = a7
    val = mem !! (add `div` 4)

-----------------------------------------------------

getReg A7 (dn, a7, mem, syms, cmp) 
 = a7
getReg r  (dn, a7, mem, syms, cmp)
 = dn !! (regToRegNumber r)

regToRegNumber D0 = 0 
regToRegNumber D1 = 1 
regToRegNumber D2 = 2 
regToRegNumber D3 = 3 
regToRegNumber D4 = 4 
regToRegNumber D5 = 5 
regToRegNumber D6 = 6 
regToRegNumber D7 = 7 
regToRegNumber A7 = error "bad register"

-----------------------------------------------------

getSymAdd x ((sym, size, add):syms)
  | x == sym  = add*4
  | otherwise = getSymAdd x syms

-----------------------------------------------------

setOpVal (Reg r) val (dn, a7, mem, syms, cmp) = (dn', a7, mem, syms, cmp)
  where
    dn' = setVal dn (regToRegNumber r) val
    
setOpVal (Reg A7) val (dn, a7, mem, syms, cmp) = (dn, val, mem, syms, cmp)
    
setOpVal (Abs name) val (dn, a7, mem, syms, cmp) = (dn, a7, mem', syms, cmp)
  where
    add = getSymAdd name syms
    mem' = setVal mem (add `div` 4) val

setOpVal (Ind A7) val (dn, a7, mem, syms, cmp) = (dn, a7, mem', syms, cmp)
  where
    add = a7
    mem' = setVal mem (add `div` 4) val

setOpVal (Ind r) val (dn, a7, mem, syms, cmp) = (dn, a7, mem', syms, cmp)
  where
    add = getReg r (dn, a7, mem, syms, cmp)
    mem' = setVal mem (add `div` 4) val


setOpVal Push val state = (dn, a7', mem', syms, cmp)
  where
    (dn, a7, mem, syms, cmp) = state
    add = a7-4
    a7' = if a7-4  0 then a7-4 else error "Stack Overflow!"
    mem' = setVal mem (add `div` 4) val

-----------------------------------------------------

allocMem :: [Int] -> Int -> [Int]
allocMem mem 0 = mem
allocMem mem size = mem ++ allocMem [0] (size-1)

setVal :: [Int] -> Int -> Int -> [Int]
setVal (x:xs) ind new
  | ind == 0    = (new:xs)
  | otherwise   = (x:(setVal xs (ind-1) new))
  
-----------------------------------------------------

printState :: State -> IO ()
printState (dn, a7, mem, syms, cmp) = 
  let
    out = "Registers D0-7: " ++ (show dn) ++ "\n" ++
          "Register  A7  : " ++ (show a7) ++ "\n" ++
          "--------------------------\n" ++
          " Variables                \n" ++
          "--------------------------\n" ++
          printSyms syms mem
  in
    putStr out

printSyms [] mem = []
printSyms ((name, size, add):syms) mem = 
  name ++ ": " ++ (show (memSection mem add size)) ++ "\n" ++
  printSyms syms mem
  
memSection [] add 0 = []
memSection (m:ms) add size
  | size == 0 = []
  | add == 0  = (m:memSection ms 0 (size-1))
  | otherwise = memSection ms (add-1) size