module States where

import Common

data State = InitialState { names :: [PlayerName] }
           | Day { players :: [Player] }
           | Night { players :: [Player] }
           | EndOfGame
           deriving (Eq)

gameHasStarted :: State -> Bool
gameHasStarted state = case state of
  (InitialState _) -> False
  _ -> True

instance Show State where
  show (InitialState names) = "InitialState\n" ++ unlines names
  show (Day players) = "Day\n" ++ (unlines $ show <$> players)
  show (Night players) = "Night\n" ++ (unlines $ show <$> players)
  show EndOfGame = "EndOfGame"
