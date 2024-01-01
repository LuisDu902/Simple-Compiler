module State where

import Element

import qualified Data.Map as Map

type State = Map.Map String Value

-- Creates an empty State
createEmptyState :: State
createEmptyState = Map.empty

-- Returns a copy of the given State with the given Value associated to the given String
push :: String -> Value -> State -> State
push = Map.insert

-- Returns the Value of the String in the given State
find :: String -> State -> Maybe Value
find = Map.lookup

-- Converts a State to a String for debugging purposes
state2Str :: State -> String
state2Str state
    | str Prelude.== "" = ""
    | otherwise = init str
        where str = fst $ Map.mapAccumWithKey (\ acc key value -> (acc ++ key ++ "=" ++ show value ++ ",", Nothing)) "" state

