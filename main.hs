import Data.Char

import qualified Element as Elm

import qualified Stack as Stk (push, pop, top)
import Stack (Stack, createEmptyStack, stack2Str)

import qualified State as Stt (push, find)
import State (State, createEmptyState, state2Str)

import qualified Text.Parsec as Parsec
import Text.ParserCombinators.Parsec ( alphaNum, lower, Parser )
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Functor.Identity


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

-- Pushes the given integer into the stack and pushes it ontop of the stack
push :: Stack -> Integer -> Stack
push stack n = Stk.push (Elm.I n) stack

-- Adds the top two elements of the stack and pushes it ontop of the stack
add :: Stack -> Stack
add stack =
  let elem1 = Stk.top stack
      elem2 = Stk.top (Stk.pop stack)
      newStack = Stk.pop (Stk.pop stack)
  in Stk.push ((Elm.+) elem1 elem2) newStack

-- Subtracts the top element of the stack with the top second element of the stack and pushes it ontop of the stack
sub :: Stack -> Stack
sub stack =
  let elem1 = Stk.top stack
      elem2 = Stk.top (Stk.pop stack)
      newStack = Stk.pop (Stk.pop stack)
  in Stk.push ((Elm.-) elem1 elem2) newStack

-- Multiplies the top two elements of the stack
mult :: Stack -> Stack
mult stack =
  let elem1 = Stk.top stack
      elem2 = Stk.top (Stk.pop stack)
      newStack = Stk.pop (Stk.pop stack)
  in Stk.push ((Elm.*) elem1 elem2) newStack

-- Pushes the boolean True onto the stack
tru :: Stack -> Stack
tru = Stk.push (Elm.B True)

-- Pushes the boolean False onto the stack
fals :: Stack -> Stack
fals = Stk.push (Elm.B False)

-- Pushes the result of the boolean and operation of the top two elements of the stack into the stack
bAnd :: Stack -> Stack
bAnd stack =
  let elem1 = Stk.top stack
      elem2 = Stk.top (Stk.pop stack)
      newStack = Stk.pop (Stk.pop stack)
  in Stk.push ((Elm.&&) elem1 elem2) newStack

-- Checks whether the top two elements of the stack are equal and pushes the result onto the stack
eq :: Stack -> Stack
eq stack =
  let elem1 = Stk.top stack
      elem2 = Stk.top (Stk.pop stack)
      newStack = Stk.pop (Stk.pop stack)
  in Stk.push ((Elm.==) elem1 elem2) newStack

-- Checks whether the top element of the stack is less or equal to the top second and pushes the result onto the stack
le :: Stack -> Stack
le stack =
  let elem1 = Stk.top stack
      elem2 = Stk.top (Stk.pop stack)
      newStack = Stk.pop (Stk.pop stack)
  in Stk.push ((Elm.<=) elem1 elem2) newStack

-- Pushes the result of boolean not operation of the top element of the stack on top of the stack
neg :: Stack -> Stack
neg stack =
  let elem = Stk.top stack
      newStack = Stk.pop stack in
  Stk.push (Elm.not elem) newStack

-- Places the value of variable with name "String" onto the stack
fetch :: Stack -> State.State -> String -> Stack
fetch stack state var =
  let value = Stt.find var state
  in case value of
    Just a -> Stk.push a stack
    Nothing -> error "Run-time error"

-- Saves the value on top of the stack into the variable named String
store :: Stack -> State.State -> String -> State.State
store stack state var =
  let newVal = Stk.top stack
  in Stt.push var newVal state

-- If the value on top of the Stack is True, runs the first piece of code given, otherwise, runs the second
branch :: (Code, Stack, State.State) -> Code -> Code -> (Code, Stack, State.State)
branch (rest, stack, state) yes no =
  case Stk.top stack of
    Elm.B True -> (yes ++ rest, Stk.pop stack, state)
    Elm.B False -> (no ++ rest, Stk.pop stack, state)
    _ -> error "Run-time error"

-- Runs the code in the condition, if the value on top of the stack is "True", runs the second part of the code and repeats the whole instruction
loop :: Code -> Code -> Code -> Code
loop rest cond body = cond ++ [Branch (body ++ [Loop cond body]) [Noop]] ++ rest

testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)


-- Part 2

data ALit = IntValue Integer | IntVariable String deriving Show

data Aexp
  = IntLit    ALit
  | IntAdd    Aexp Aexp
  | IntMult   Aexp Aexp
  | IntSub    Aexp Aexp

data Bexp
  = BoolLit    Bool
  | BoolNeg    Bexp
  | BoolAnd    Bexp Bexp
  | BoolEqual  Bexp Bexp
  | IntEqual   Aexp Aexp
  | IntLe      Aexp Aexp

data Stm
  = IfStm Bexp [Stm] [Stm]
  | LoopStm Bexp [Stm]
  | AssignStm String Aexp
  | SequenceOfStm [Stm]

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

-- Definition of reserved words and symbols
languageDefinition :: GenLanguageDef String u Data.Functor.Identity.Identity
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

lexer :: Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser languageDefinition


variable :: Parser String
variable = Token.identifier lexer
reserved :: String -> Parsec.ParsecT String u Data.Functor.Identity.Identity ()
reserved   = Token.reserved   lexer
reservedOp :: String -> Parsec.ParsecT String u Data.Functor.Identity.Identity ()
reservedOp = Token.reservedOp lexer
parens :: Parsec.ParsecT String u Data.Functor.Identity.Identity a -> Parsec.ParsecT String u Data.Functor.Identity.Identity a
parens     = Token.parens     lexer
integer :: Parsec.ParsecT String u Data.Functor.Identity.Identity Integer
integer    = Token.integer    lexer
semiColon :: Parsec.ParsecT String u Data.Functor.Identity.Identity String
semiColon       = Token.semi       lexer
whiteSpace :: Parsec.ParsecT String u Data.Functor.Identity.Identity ()
whiteSpace = Token.whiteSpace lexer

-- Handles arithmetic expressions
aritExp :: Parser Aexp
aritExp = buildExpressionParser aOperators aritParser

-- Handles boolean expressions
boolExp :: Parser Bexp
boolExp = buildExpressionParser bOperators boolParser

-- Handles operators for arithmetic expressions
aOperators :: [[Operator Char st Aexp]]
aOperators = [ [Infix  (reservedOp "*"   >> return IntMult) AssocLeft]
             , [Infix  (reservedOp "+"   >> return IntAdd) AssocLeft,
                Infix  (reservedOp "-"   >> return IntSub) AssocLeft]
              ]

-- Handles operators for boolean expressions
bOperators :: [[Operator Char st Bexp]]
bOperators = [ [Prefix (reservedOp "not" >> return BoolNeg)          ],
                [Infix (reservedOp "=" >> return BoolEqual) AssocLeft         ],
              [Infix  (reservedOp "and" >> return BoolAnd) AssocLeft]
             ]


intParser :: Parser ALit
intParser = fmap IntValue integer Parsec.<|> fmap IntVariable variable

aritParser :: Parser Aexp
aritParser =  parens aritExp Parsec.<|> fmap IntLit intParser

boolParser :: Parser Bexp
boolParser =  parens boolExp
     Parsec.<|> (reserved "True"  >> return (BoolLit True) )
     Parsec.<|> (reserved "False" >> return (BoolLit False) )
     Parsec.<|> intCompareParser

intCompareParser :: Parser Bexp
intCompareParser =
   do a1 <- aritExp
      op <- comp
      a2 <- aritExp
      return $ op a1 a2

comp :: Parser (Aexp -> Aexp -> Bexp)
comp = (reservedOp "<=" >> return IntLe) Parsec.<|> (reservedOp "==" >> return IntEqual)




statementsParser :: Parser [Stm]
statementsParser = parens statementsParser Parsec.<|> Parsec.many statementParser 

blockParser :: Parser [Stm]
blockParser = parens statementsParser <* semiColon Parsec.<|> fmap (:[]) statementParser

thenParser :: Parser [Stm]
thenParser = parens statementsParser Parsec.<|> fmap (:[]) statementParser

statementParser :: Parser Stm
statementParser =  parens statementParser
           Parsec.<|> ifParser
           Parsec.<|> loopParser
           Parsec.<|> assignParser

ifParser :: Parser Stm
ifParser =
  do  reserved "if"
      cond  <- boolExp
      reserved "then"
      ifBlock <- thenParser
      reserved "else"
      elseBlock <- blockParser
      return (IfStm cond ifBlock elseBlock)


loopParser :: Parser Stm
loopParser =
  do  reserved "while"
      cond <- boolExp
      reserved "do"
      loopBody <- blockParser
      return (LoopStm cond loopBody)

assignParser :: Parser Stm
assignParser =
  do var  <- variable
     reservedOp ":="
     value <- aritExp
     semiColon
     return (AssignStm var value)



parse :: String -> Program
parse str =
  case Parsec.parse (whiteSpace >> statementsParser <* Parsec.eof) "" str of
    Left e -> error "Run-time error"
    Right r -> r

testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (compile (parse programCode), createEmptyStack, createEmptyState)


a = testParser "x := 5; x := x - 1;" == ("","x=4")
b = testParser "x := 0 - 2;" == ("","x=-2")
c = testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
d = testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
e = testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
f = testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
g = testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
h = testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
i = testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
j = testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
k = testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
l = testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
m = testParser "           z := 2;    if False   =False and True then (    if (z == 2) then why := 3; else (a := 2;); x := 2;) else x := 10;" 


