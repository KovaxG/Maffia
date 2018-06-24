module States where

import Common

data State = InitialState { names :: [PlayerName] }
           | Day { players :: [Player] }
           | Night { players :: [Player] }
           | EndOfGame
           deriving (Show, Eq)

gameHasStarted :: State -> Bool
gameHasStarted state = case state of
  (InitialState _) -> False
  _ -> True
