module Stack where

data Value =
  Int Integer | Bl Bool

instance Show Value where
    show (Int var) = show var
    show (Bl var) = show var

type Stack = [Value]

createEmptyStack :: Stack
createEmptyStack = []

push :: Value -> Stack -> Stack
push a [] = a:[]
push a s = a:s

pop :: Stack -> (Maybe Value, Stack)
pop [] = (Nothing, [])
pop (a:s) = (Just a, s)

stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str (a:[]) = show a
stack2Str (a:s) = show a ++ "," ++ (stack2Str s)
