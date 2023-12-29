module Stack where

import Element

type Stack = [Value]

createEmptyStack :: Stack
createEmptyStack = []

push :: Value -> Stack -> Stack
push a [] = [a]
push a s = a:s

pop :: Stack -> Stack
pop (a:s) = s
pop _ = error "Stack.pop: the stack is empty"

top :: Stack -> Value
top (a:s) = a
top _ = error "Stack.top: the stack is empty"

stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str [a] = show a
stack2Str (a:s) = show a ++ "," ++ stack2Str s

size :: Stack -> Int
size = length