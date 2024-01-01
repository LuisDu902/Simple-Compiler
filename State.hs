module State where

import Element

import qualified Data.Map as Map

type State = Map.Map String Value

createEmptyState :: State
createEmptyState = Map.empty

push :: String -> Value -> State -> State
push = Map.insert

find :: String -> State -> Maybe Value
find = Map.lookup

state2Str :: State -> String
state2Str state
    | str Prelude.== "" = ""
    | otherwise = init str
        where str = fst $ Map.mapAccumWithKey (\ acc key value -> (acc ++ key ++ "=" ++ show value ++ ",", Nothing)) "" state

