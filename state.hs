module State where

import qualified Data.Map as Map

data Value =
  Int Integer | Bl Bool

instance Show Value where
    show (Int var) = show var
    show (Bl var) = show var

type State = Map.Map String Value

createEmptyState :: State
createEmptyState = Map.empty

push :: String -> Value -> State -> State
push var value state = Map.insert var value state

-- state2Str :: State -> String
state2Str = undefined

find :: String -> State -> Maybe Value
find var state = Map.lookup var state
