module State where

import Element

import qualified Data.Map as Map

type State = Map.Map String Value

createEmptyState :: State
createEmptyState = Map.empty

push :: String -> Value -> State -> State
push var value state = Map.insert var value state

state2Str :: State -> String
state2Str = undefined
{-
state2Str _ = []
state2Str (a:[]) = show a
state2Str (a:s) = show a ++ "," ++ (state2Str s)
-}

find :: String -> State -> Maybe Value
find var state = Map.lookup var state
