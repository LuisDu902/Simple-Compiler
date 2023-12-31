import Data.Char

import qualified Element as Elm

import qualified Stack as Stk (push, pop, top)
import Stack (Stack, createEmptyStack, stack2Str)

import qualified State as Stt (push, find)
import State (State, createEmptyState, state2Str)

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec ( alphaNum, lower, Parser )
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Text.Parsec as Parsec

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]


run :: (Code, Stack, State.State) -> (Code, Stack, State.State)
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
tru = Stk.push (Elm.B True)

fals :: Stack -> Stack
fals = Stk.push (Elm.B False)

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

fetch :: Stack -> State.State -> String -> Stack
fetch stack state var =
  let value = Stt.find var state
  in case value of
    Just a -> Stk.push a stack
    Nothing -> error "Runtime error"

store :: Stack -> State.State -> String -> State.State
store stack state var =
  let newVal = Stk.top stack
  in Stt.push var newVal state

branch :: (Code, Stack, State.State) -> Code -> Code -> (Code, Stack, State.State)
branch (rest, stack, state) yes no =
  case Stk.top stack of
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


data ALit = IntValue Integer | IntVariable String deriving Show

data Aexp
  = IntLit    ALit
  | IntAdd    Aexp Aexp
  | IntMult   Aexp Aexp
  | IntSub    Aexp Aexp
  deriving Show

data Bexp
  = BoolLit    Bool
  | BoolNeg    Bexp
  | BoolAnd    Bexp Bexp
  | BoolEqual  Bexp Bexp
  | IntEqual   Aexp Aexp
  | IntLe      Aexp Aexp
  deriving Show

data Stm
  = IfStm Bexp [Stm] [Stm]
  | LoopStm Bexp [Stm]
  | AssignStm String Aexp
  deriving Show

type Program = [Stm]

compA :: Aexp -> Code
compA (IntLit (IntValue n)) = [Push n]
compA (IntLit (IntVariable var)) = [Fetch var]
compA (IntAdd exp1 exp2) = compA exp2 ++ compA exp1 ++ [Add]
compA (IntMult exp1 exp2) = compA exp2 ++ compA exp1 ++ [Mult]
compA (IntSub exp1 exp2) = compA exp2 ++ compA exp1 ++ [Sub]

compB :: Bexp -> Code
compB (BoolLit n)
  | n         = [Tru]
  | otherwise = [Fals]
compB (BoolNeg exp) = compB exp ++ [Neg]
compB (BoolAnd exp1 exp2) = compB exp2 ++ compB exp1 ++ [And]
compB (BoolEqual exp1 exp2) = compB exp2 ++ compB exp1 ++ [Equ]
compB (IntEqual exp1 exp2) = compA exp2 ++ compA exp1 ++ [Equ]
compB (IntLe exp1 exp2) = compA exp2 ++ compA exp1 ++ [Le]

compile :: Program -> Code
compile [] = []
compile (statement : rest) =
  case statement of
    AssignStm var aExp -> compA aExp ++ [Store var] ++ compile rest
    IfStm cond ifBlock elseBlock -> compB cond ++ [Branch (compile ifBlock) (compile elseBlock)] ++ compile rest
    LoopStm cond loopBody -> Loop (compB cond) (compile loopBody) : compile rest

languageDefinition =
   emptyDef { Token.identStart      = lower
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "if"
                                      , "then"
                                      , "else"
                                      , "while"
                                      , "do"
                                      , "True"
                                      , "False"
                                      , "not"
                                      , "and"
                                      ]
            , Token.reservedOpNames = ["+", "-", "*"
                                      ,"==", "=", "<=", "and", "not"
                                      , ":="
                                      ]
            }

lexer = Token.makeTokenParser languageDefinition


variable = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
semiColon       = Token.semi       lexer
whiteSpace = Token.whiteSpace lexer


statementsParser = do
  list <- Parsec.sepEndBy statementParser semiColon
  return $ list

statementParser :: Parser Stm
statementParser =  parens statementParser 
           Parsec.<|> ifParser
           Parsec.<|> loopParser
           Parsec.<|> assignParser


ifParser :: Parser Stm
ifParser =
  do reserved "if"
     cond  <- boolExp
     reserved "then"
     ifBlock <- statementsParser
     reserved "else"
     elseBlock <- statementParser
     return (IfStm cond ifBlock [elseBlock])


loopParser :: Parser Stm
loopParser = do
  reserved "while"
  cond <- boolExp
  reserved "do"
  hasParens <- Parsec.lookAhead (Parsec.char '(')
  loopBody <- if hasParens == '('
               then do
                 body <- statementsParser <* Parsec.char ')'
                 return (LoopStm cond body)
               else do
                 body <- statementParser
                 return (LoopStm cond [body])
  return loopBody

assignParser :: Parser Stm
assignParser =
  do var  <- variable
     reservedOp ":="
     value <- aritExp
     return (AssignStm var value)

aritExp :: Parser Aexp
aritExp = buildExpressionParser aOperators aTerm

boolExp :: Parser Bexp
boolExp = buildExpressionParser bOperators bTerm


aOperators = [ [Infix  (reservedOp "*"   >> return IntMult) AssocLeft]
             , [Infix  (reservedOp "+"   >> return IntAdd) AssocLeft,
                Infix  (reservedOp "-"   >> return IntSub) AssocLeft]
              ]

bOperators = [ [Prefix (reservedOp "not" >> return BoolNeg)          ],
                [Infix (reservedOp "=" >> return BoolEqual) AssocLeft         ],
              [Infix  (reservedOp "and" >> return BoolAnd) AssocLeft]
             ]


intParser :: Parser ALit
intParser = fmap IntValue integer Parsec.<|> fmap IntVariable variable


aTerm =  parens aritExp
      Parsec.<|> fmap IntLit intParser
bTerm =  parens boolExp
     Parsec.<|> (reserved "True"  >> return (BoolLit True) )
     Parsec.<|> (reserved "False" >> return (BoolLit False) )
     Parsec.<|> intCompareParser


intCompareParser =
   do a1 <- aritExp
      op <- comp
      a2 <- aritExp
      return $ op a1 a2

comp =   (reservedOp "<=" >> return IntLe)
          Parsec.<|> (reservedOp "==" >> return IntEqual)

myParser = whiteSpace >> statementsParser


parse :: String -> Program
parse str =
  case Parsec.parse (myParser <* Parsec.eof) "" str of
    Left e  -> error "Runtime error"
    Right r -> r

testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)


a = testParser "x := 5; x := x - 1;" == ("","x=4") 
b = testParser "x := 0 - 2;" == ("","x=-2") 
c = testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")


-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1") x
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2") x
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4") x
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68") x
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
d = testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
e = testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
f = testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")

-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")