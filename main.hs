import Data.Char

import qualified Element as Elm

import qualified Stack as Stk (push, pop, top)
import Stack (Stack, createEmptyStack, stack2Str)

import qualified State as Stt (push, find)
import State (State, createEmptyState, state2Str)


-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst] 


run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)

run (Push n : rest, stack, state) = run (rest, push stack n, state)
run (Tru : rest, stack, state) = run (rest, tru stack, state)
run (Fals : rest, stack, state) = run (rest, fals stack, state)

run (Add : rest, stack, state) = run (rest, add stack, state)
run (Mult : rest, stack, state) = run (rest, mult stack, state)
run (Sub : rest, stack, state) = run (rest, sub stack, state)

run (Equ : rest, stack, state) = run (rest, eq stack, state)
run (Le : rest, stack, state) = run (rest, le stack, state)
run (And : rest, stack, state) = run (rest, bAnd stack, state)
run (Neg : rest, stack, state) = run (rest, neg stack, state)

run (Fetch var : rest, stack, state) = run (rest, fetch stack state var, state)
run (Store var : rest, stack, state) = run (rest, Stk.pop stack, store stack state var)
run (Noop : rest, stack, state) = run (rest, stack, state)

run (Branch yes no : rest, stack, state) = run (branch (rest, stack, state) yes no)
run (Loop cond body : rest, stack, state) = run (loop rest cond body, stack, state)


push :: Stack -> Integer -> Stack
push stack n = Stk.push (Elm.I n) stack

add :: Stack -> Stack 
add stack = 
  let elem1 = Stk.top stack 
      elem2 = Stk.top (Stk.pop stack)
      newStack = Stk.pop (Stk.pop stack)
  in Stk.push ((Elm.+) elem1 elem2) newStack

sub :: Stack -> Stack 
sub stack = 
  let elem1 = Stk.top stack 
      elem2 = Stk.top (Stk.pop stack)
      newStack = Stk.pop (Stk.pop stack)
  in Stk.push ((Elm.-) elem1 elem2) newStack

mult :: Stack -> Stack
mult stack = 
  let elem1 = Stk.top stack 
      elem2 = Stk.top (Stk.pop stack)
      newStack = Stk.pop (Stk.pop stack)
  in Stk.push ((Elm.*) elem1 elem2) newStack

tru :: Stack -> Stack
tru stack = Stk.push (Elm.B True) stack

fals :: Stack -> Stack
fals stack = Stk.push (Elm.B False) stack

bAnd :: Stack -> Stack
bAnd stack = 
  let elem1 = Stk.top stack 
      elem2 = Stk.top (Stk.pop stack)
      newStack = Stk.pop (Stk.pop stack)
  in Stk.push ((Elm.&&) elem1 elem2) newStack

eq :: Stack -> Stack
eq stack = 
  let elem1 = Stk.top stack 
      elem2 = Stk.top (Stk.pop stack)
      newStack = Stk.pop (Stk.pop stack)
  in Stk.push ((Elm.==) elem1 elem2) newStack

le :: Stack -> Stack
le stack = 
  let elem1 = Stk.top stack 
      elem2 = Stk.top (Stk.pop stack)
      newStack = Stk.pop (Stk.pop stack)
  in Stk.push ((Elm.<=) elem1 elem2) newStack

neg :: Stack -> Stack
neg stack = 
  let elem = Stk.top stack 
      newStack = Stk.pop stack in 
  Stk.push (Elm.not elem) newStack

fetch :: Stack -> State -> String -> Stack
fetch stack state var = 
  let value = Stt.find var state
  in case value of 
    Just a -> Stk.push a stack
    Nothing -> error "Runtime error"

store :: Stack -> State -> String -> State
store stack state var = 
  let newVal = Stk.top stack 
  in Stt.push var newVal state 
 
branch :: (Code, Stack, State) -> Code -> Code -> (Code, Stack, State)
branch (rest, stack, state) yes no = 
  case (Stk.top stack) of 
    Elm.B True -> (yes ++ rest, Stk.pop stack, state)
    Elm.B False -> (no ++ rest, Stk.pop stack, state)
    _ -> error "Runtime error"

loop :: Code -> Code -> Code -> Code
loop rest cond body = cond ++ [Branch (body ++ [Loop cond body]) [Noop]] ++ rest


-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)




-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program


-- testAssembler ] == ("","fact=3628800,i=1")


