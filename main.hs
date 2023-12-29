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
-- run ([Add,rest], stack, state) = add(rest,stack,state)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)

add :: (Code, Stack, State) -> (Code, Stack, State) 
add (code, stack, state) = (code, Stk.push ((Elm.+) (Stk.top stack) (Stk.top (Stk.pop stack))) (Stk.pop (Stk.pop stack)), state)

sub :: (Code, Stack, State) -> (Code, Stack, State) 
sub (code, stack, state) = (code, Stk.push ((Elm.-) (Stk.top stack) (Stk.top (Stk.pop stack))) (Stk.pop (Stk.pop stack)), state)

mul :: (Code, Stack, State) -> (Code, Stack, State) 
mul (code, stack, state) = (code, Stk.push ((Elm.*) (Stk.top stack) (Stk.top (Stk.pop stack))) (Stk.pop (Stk.pop stack)), state)

eq :: (Code, Stack, State) -> (Code, Stack, State)
eq (code, stack, state) = (code, Stk.push ((Elm.==) (Stk.top stack) (Stk.top (Stk.pop stack))) (Stk.pop (Stk.pop stack)), state)

le :: (Code, Stack, State) -> (Code, Stack, State)
le (code, stack, state) = (code, Stk.push ((Elm.<=) (Stk.top stack) (Stk.top (Stk.pop stack))) (Stk.pop (Stk.pop stack)), state)

tru :: (Code, Stack, State) -> (Code, Stack, State)
tru (code, stack, state) = (code, Stk.push (Elm.B True) stack, state)

fals :: (Code, Stack, State) -> (Code, Stack, State) 
fals (code, stack, state) = (code, Stk.push (Elm.B False) stack, state)

push :: (Code, Stack, State) -> Integer -> (Code, Stack, State)
push (code, stack, state) v = (code, Stk.push (Elm.I v) stack, state)

neg :: (Code, Stack, State) -> (Code, Stack, State)
neg (code, stack, state) = let s2 = Stk.pop stack in 
  (code,Stk.push (Elm.not (Stk.top stack)) s2, state)

fetch :: (Code, Stack, State) -> String -> (Code, Stack, State)
fetch (code, stack, state) str = let n1 = Stk.top stack in 
  let newVal = Stt.find str state in 
  case newVal of 
    Just a -> (code, Stk.push a stack, state)
    Nothing -> error "Runtime error"

store :: (Code, Stack, State) -> String -> (Code, Stack, State)
store (code, stack, state) str = let newVal = Stk.top stack in 
  let newState = Stt.push str newVal state in 
  (code, Stk.pop stack, newState)

branch :: (Code, Stack, State) -> Code -> Code -> (Code, Stack, State)
branch (code, stack, state) yes no = case (Stk.top stack) of 
  Elm.B True -> (yes ++ code, Stk.pop stack, state)
  Elm.B False -> (no ++ code, Stk.pop stack, state)
  _ -> error "Runtime error"

noop :: (Code, Stack, State) -> (Code, Stack, State)
noop (code, stack, state) = (code, stack, state)

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

