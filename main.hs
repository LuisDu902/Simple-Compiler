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

-- Runs the list of instructions returning as ouput an empty code list, a stack and the output values in the storage
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

-- Definition of arithmetic literals data type representing constants (IntValue) and variables (IntVariable).
data ALit = IntValue Integer | IntVariable String deriving Show

-- Definition of arithmetic expressions data type 
data Aexp
  = IntLit    ALit
  | IntAdd    Aexp Aexp
  | IntMult   Aexp Aexp
  | IntSub    Aexp Aexp

-- Definition of boolean expressions data type 
data Bexp
  = BoolLit    Bool
  | BoolNeg    Bexp
  | BoolAnd    Bexp Bexp
  | BoolEqual  Bexp Bexp
  | IntEqual   Aexp Aexp
  | IntLe      Aexp Aexp

-- Definition of statements data type 
data Stm
  = IfStm Bexp [Stm] [Stm]
  | LoopStm Bexp [Stm]
  | AssignStm String Aexp
  | SequenceOfStm [Stm]

-- Definition of program, i.e., a list of statements
type Program = [Stm]


-------------
-- Compile --
-------------

-- Compiles arithmetic expressions (Aexp) into a sequence of instructions (Code)
compA :: Aexp -> Code
compA (IntLit (IntValue n)) = [Push n]
compA (IntLit (IntVariable var)) = [Fetch var]
compA (IntAdd exp1 exp2) = compA exp2 ++ compA exp1 ++ [Add]
compA (IntMult exp1 exp2) = compA exp2 ++ compA exp1 ++ [Mult]
compA (IntSub exp1 exp2) = compA exp2 ++ compA exp1 ++ [Sub]

-- Compiles boolean expressions (Bexp) into a sequence of instructions (Code)
compB :: Bexp -> Code
compB (BoolLit n)
  | n         = [Tru]
  | otherwise = [Fals]
compB (BoolNeg exp) = compB exp ++ [Neg]
compB (BoolAnd exp1 exp2) = compB exp2 ++ compB exp1 ++ [And]
compB (BoolEqual exp1 exp2) = compB exp2 ++ compB exp1 ++ [Equ]
compB (IntEqual exp1 exp2) = compA exp2 ++ compA exp1 ++ [Equ]
compB (IntLe exp1 exp2) = compA exp2 ++ compA exp1 ++ [Le]

-- Compiles a program (list of statements) into a sequence of instructions (Code)
compile :: Program -> Code
compile [] = []
compile (statement : rest) =
  case statement of
    AssignStm var aExp -> compA aExp ++ [Store var] ++ compile rest
    IfStm cond ifBlock elseBlock -> compB cond ++ [Branch (compile ifBlock) (compile elseBlock)] ++ compile rest
    LoopStm cond loopBody -> Loop (compB cond) (compile loopBody) : compile rest



-----------
-- Lexer --
-----------

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

-- Definition of a lexer using the defined language 
lexer :: Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser languageDefinition



-------------------
-- Token Parsers --
-------------------

-- Extraction of Token Parsers
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

------------------------
-- Expression Parsers --
------------------------

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

-- Parser for arithmetic literals 
intParser :: Parser ALit
intParser = fmap IntValue integer Parsec.<|> fmap IntVariable variable

-- Parser for arithmetic terms 
aritParser :: Parser Aexp
aritParser =  parens aritExp Parsec.<|> fmap IntLit intParser

-- Parser for boolean terms 
boolParser :: Parser Bexp
boolParser =  parens boolExp
     Parsec.<|> (reserved "True"  >> return (BoolLit True) )
     Parsec.<|> (reserved "False" >> return (BoolLit False) )
     Parsec.<|> intCompareParser

-- Handles boolean expressions that compare arithmetic expressions
intCompareParser :: Parser Bexp
intCompareParser =
   do a1 <- aritExp
      op <- comp
      a2 <- aritExp
      return $ op a1 a2

-- Handles operators for boolean expressions that compare arithmetic expressions
comp :: Parser (Aexp -> Aexp -> Bexp)
comp = (reservedOp "<=" >> return IntLe) Parsec.<|> (reservedOp "==" >> return IntEqual)



-----------------------
-- Statement Parsers --
-----------------------


-- Parser for different types of statements: if statements, loops, and assignment statements
statementParser :: Parser Stm
statementParser =  parens statementParser
           Parsec.<|> ifParser
           Parsec.<|> loopParser
           Parsec.<|> assignParser

-- Parses an if statement with its condition, then block, and else block
ifParser :: Parser Stm
ifParser =
  do  reserved "if"
      cond  <- boolExp
      reserved "then"
      ifBlock <- thenParser
      reserved "else"
      elseBlock <- blockParser
      return (IfStm cond ifBlock elseBlock)

-- Parses a loop statement with its condition and the body of the loop
loopParser :: Parser Stm
loopParser =
  do  reserved "while"
      cond <- boolExp
      reserved "do"
      loopBody <- blockParser
      return (LoopStm cond loopBody)

-- Parses an assignment statement assigning a value to a variable
assignParser :: Parser Stm
assignParser =
  do var  <- variable
     reservedOp ":="
     value <- aritExp
     semiColon
     return (AssignStm var value)


------------------------
-- Statements Parsers --
------------------------

-- Parses a list of statements
statementsParser :: Parser [Stm]
statementsParser = parens statementsParser Parsec.<|> Parsec.many statementParser 

-- Parses a block of statements, ending with a semicolon
blockParser :: Parser [Stm]
blockParser = parens statementsParser <* semiColon Parsec.<|> fmap (:[]) statementParser

-- Parses a 'then' block, allowing for either multiple statements within parentheses or a single statement
thenParser :: Parser [Stm]
thenParser = parens statementsParser Parsec.<|> fmap (:[]) statementParser


-----------
-- Parse --
-----------

-- Parses a given string, returning a list of statemements (Program)
parse :: String -> Program
parse str =
  case Parsec.parse (whiteSpace >> statementsParser <* Parsec.eof) "" str of
    Left e -> error "Run-time error"
    Right r -> r

testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (compile (parse programCode), createEmptyStack, createEmptyState)

main :: IO ()
main = do
  putStrLn "Running testAssembler tests:"
  let a = testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
  let b = testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
  let c = testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
  let d = testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
  let e = testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
  let f = testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
  let g = testAssembler [Push (-20),Push (-21), Le] == ("True","")
  let h = testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
  let i = testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
  -- If you test:
  -- testAssembler [Push 1,Push 2,And]
  -- You should get an exception with the string: "Run-time error"
  -- If you test:
  -- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
  -- You should get an exception with the string: "Run-time error"

  let results = [ ("Test 1", a)
                , ("Test 2", b)
                , ("Test 3", c)
                , ("Test 4", d)
                , ("Test 5", e)
                , ("Test 6", f)
                , ("Test 7", g)
                , ("Test 8", h)
                , ("Test 9", i)
                ]

  mapM_ (\(testName, result) -> putStrLn $ testName ++ ": " ++ if result then "Passed" else "Failed") results
  putStrLn "Running testParser tests:"
  let a1 = testParser "x := 5; x := x - 1;" == ("","x=4")
  let b1 = testParser "x := 0 - 2;" == ("","x=-2")
  let c1 = testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
  let d1 = testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
  let e1 =  testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
  let f1 =  testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
  let g1 =  testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
  let h1 =  testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
  let i1 =  testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
  let j1 =  testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
  let k1 =  testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
  let l1 =  testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")


  let results2 = [ ("Test 1", a1)
                , ("Test 2", b1)
                , ("Test 3", c1)
                , ("Test 4", d1)
                , ("Test 5", e1)
                , ("Test 6", f1)
                , ("Test 7", g1)
                , ("Test 8", h1)
                , ("Test 9", i1)
                , ("Test 10", j1)
                , ("Test 11", k1)
                , ("Test 12", l1)
                ]

  mapM_ (\(testName, result) -> putStrLn $ testName ++ ": " ++ if result then "Passed" else "Failed") results2
