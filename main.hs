import Data.Char

import qualified Stack as Stack (push, pop)
import Stack (Stack, createEmptyStack, stack2Str)

import qualified State as State (push, find)
import State (State, createEmptyState, state2Str)

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]


-- run :: (Code, Stack, State) -> (Code, Stack, State)
run = undefined -- TODO

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



data Aexp
  = IntValue  Integer
  | IntVariable  String
  | IntAdd    Aexp Aexp
  | IntMult   Aexp Aexp
  | IntSub    Aexp Aexp


data Bexp
  = BoolValue  Bool
  | IntValue2  Integer
  | BoolVariable  String
  | IntEqual  Bexp Bexp
  | IntLe     Bexp Bexp
  | BoolNeg    Bexp
  | BoolAnd    Bexp Bexp
  | BoolEqual  Bexp Bexp

data Stm
  = IfStm Bexp [Stm] [Stm]
  | LoopStm Bexp [Stm]
  | AssignIntStm String Aexp
  | AssignBoolStm String Bexp

type Program = [Stm]

compA :: Aexp -> Code
compA (IntValue n) = [Push n]
compA (IntVariable var) = [Fetch var]
compA (IntAdd exp1 exp2) = compA exp2 ++ compA exp1 ++ [Add]
compA (IntMult exp1 exp2) = compA exp2 ++ compA exp1 ++ [Mult]
compA (IntSub exp1 exp2) = compA exp2 ++ compA exp1 ++ [Sub]

compB :: Bexp -> Code
compB (BoolValue True) = [Tru]
compB (BoolValue False) = [Fals]
compB (BoolVariable var) = [Fetch var]
compB (IntValue2 n) = [Push n]
compB (BoolNeg exp) = compB exp ++ [Neg]
compB (BoolAnd exp1 exp2) = compB exp2 ++ compB exp1 ++ [And]
compB (BoolEqual exp1 exp2) = compB exp2 ++ compB exp1 ++ [Equ]
compB (IntEqual exp1 exp2) = compB exp2 ++ compB exp1 ++ [Equ]
compB (IntLe exp1 exp2) = compB exp2 ++ compB exp1 ++ [Le]

compile :: Program -> Code
compile [] = []

compile (statement : rest) =
  case statement of
    AssignIntStm var aExp -> compA aExp ++ [Store var] ++ compile rest
    AssignBoolStm var bExp -> compB bExp ++ [Store var] ++ compile rest
    --IfStm bexp ifBlock elseBlock -> compA aExp ++ [Store var] ++ compile rest
    LoopStm bExp loopBody -> Loop (compB bExp) (compile loopBody) : compile rest

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
--testParser :: String -> (String, String)
--testParser programCode = (stack2Str stack, store2Str store)
--  where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)



data Token = PlusTok
          | TimesTok
          | OpenTok
          | CloseTok
          | SubTok
          | IntEqualTok
          | LessEqualTok
          | BoolEqualTok
          | AndTok
          | NegTok
          | AssignTok
          | IfTok
          | ThenTok
          | ElseTok
          | WhileTok
          | DoTok
          | EndTok
          | IntTok Int
          | BoolTok Bool
          | VarTok String
          deriving (Show)

lexer :: String -> [Token]
lexer [] = []

-- Symbols
lexer (';' : rest) = EndTok : lexer rest
lexer ('+' : rest) = PlusTok : lexer rest
lexer ('-' : rest) = SubTok : lexer rest
lexer ('*' : rest) = TimesTok : lexer rest
lexer ('(' : rest) = OpenTok : lexer rest
lexer (')' : rest) = CloseTok : lexer rest
lexer (':' : '=' : rest) = AssignTok : lexer rest
lexer ('<' : '=' : rest) = LessEqualTok : lexer rest
lexer ('=' : '=' : rest) = IntEqualTok : lexer rest
lexer ('=' : rest) = BoolEqualTok : lexer rest

-- If statements
lexer ('i' : 'f' : rest) = IfTok : lexer rest
lexer ('t' : 'h' : 'e': 'n' : rest) = ThenTok : lexer rest
lexer ('e' : 'l' : 's': 'e' : rest) = ElseTok : lexer rest

-- Boolean operations
lexer ('n' : 'o' : 't': rest) = NegTok : lexer rest
lexer ('a' : 'n' : 'd': rest) = AndTok : lexer rest

-- While loops
lexer ('w' : 'h' : 'i': 'l': 'e' : rest) = WhileTok : lexer rest
lexer ('d' : 'o' : rest) = DoTok : lexer rest

-- Boolean values
lexer ('T' : 'r' : 'u': 'e' : rest) = BoolTok True : lexer rest
lexer ('F' : 'a' : 'l': 's': 'e' : rest) = BoolTok False : lexer rest

-- Space
lexer (chr : rest)
    | isSpace chr
    = lexer rest

-- Integer values
lexer str@(chr : _)
    | isDigit chr
    = IntTok (stringToInt integer) : lexer rest
    where
      (integer, rest) = span isDigit str
      stringToInt :: String -> Int
      stringToInt=foldl (\acc chr->10*acc+digitToInt chr) 0

-- Variables
lexer str@(chr : _)
    | isLower chr
    = VarTok variable : lexer rest
    where
      (variable, rest) = span isVariable str
      isVariable :: Char -> Bool
      isVariable c = not (isSpace c) && isAlphaNum c && c `notElem` ['+', '-', '*', ':', '=', ';', '<', ')', '(']

-- Error handling
lexer (chr : rest) = error ("unexpected character: ’" ++ show (chr) ++ "’")

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

testProgram :: Program
-- y := 1; while ¬(x = 1) do (y := y ∗ x; x := x − 1)
testProgram = [AssignIntStm "y" (IntValue 1), LoopStm (BoolNeg (IntEqual (BoolVariable "x") (IntValue2 1))) [AssignIntStm "y" (IntMult (IntVariable "y") (IntVariable "x")), AssignIntStm "x" (IntSub (IntVariable "x") (IntValue 1))]]
--testProgram = [AssignBoolStm "x" (BoolNeg (BoolEqual (BoolVariable "x") (BoolValue True)))]
main :: IO ()
main = do
    let compiledCode = compile testProgram
    print compiledCode


