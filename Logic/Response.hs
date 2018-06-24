module Response where

data Response = UndefinedTransition
              | PlayerAdded
              | PlayerNameTaken
              | GameStarted
              | NeedMorePlayers
              | EndOfDay
              | EndOfNight
              | NoSuchPlayer
              | PlayerRoleIs String
              | MaffiaTargetSuccess
              | MaffiaWins
              | TownWins
              deriving (Show, Eq)

toMessage :: Response -> String
toMessage response = case response of
  UndefinedTransition -> "That message does not make sense for the current state."
  PlayerAdded -> "Player has been added."
  PlayerNameTaken -> "That name is already taken."
  GameStarted -> "The game has been started. Good luck."
  NeedMorePlayers -> "You need to be at least 5 to start playing the game!"
  EndOfDay -> "The city now goes to sleep ..."
  EndOfNight -> "The city now wakes up ..."
  NoSuchPlayer -> "That player does not exist!"
  PlayerRoleIs role -> "The role of the player is: " ++ role
  MaffiaTargetSuccess -> "Player has been selected."
  MaffiaWins -> "The maffia has won the game!"
  TownWins -> "The tow has won the game!"
