module Stack where

import Element

type Stack = [Value]

-- Creates and returns an empty stack
createEmptyStack :: Stack
createEmptyStack = []

-- Returns a copy of the given stack with the given Value on top
push :: Value -> Stack -> Stack
push a [] = [a]
push a s = a:s

-- Returns a copy of the given stack with the top element removed
pop :: Stack -> Stack
pop (a:s) = s
pop _ = error "Run-time error"

-- Returns the Value that is currently on top of the given Stack
top :: Stack -> Value
top (a:s) = a
top _ = error "Run-time error"

-- Converts a Stack into a string for debugging purposes
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str [a] = show a
stack2Str (a:s) = show a ++ "," ++ stack2Str s

-- Returns the size of the given Stack
size :: Stack -> Int
size = length