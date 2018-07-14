module Logic.Response where

import Logic.Common

data Response = BroadCast Public
              | Feedback Private
              | TeamMessage Maffia
              deriving (Show, Eq)

data Private = UndefinedTransition
             | PlayerNameTaken
             | NoSuchPlayer
             | PlayerRoleIs Role
             | DoctorTargetSuccess
             | InvestigationLimit
             | AlreadyLynched
             deriving (Show, Eq)

data Public = PlayerAdded
            | GameStarted
            | NeedMorePlayers
            | EndOfDay
            | EndOfNight
            | MaffiaWins
            | TownWins
            | VoteCast
            | VoteCancelled
            | PlayerDied PlayerName
            | PlayerLynched PlayerName
            deriving (Show, Eq)

data Maffia = MaffiaTargetSuccess deriving (Show, Eq)

isBroadcast :: Response -> Bool
isBroadcast (BroadCast _) = True
isBroadcast _ = False

toMessage :: Response -> String
toMessage response = case response of
  (BroadCast msg) -> encodePublic msg
  (Feedback msg) -> encodePrivate msg
  (TeamMessage msg) -> encodeMaffia msg

encodePrivate :: Private -> String
encodePrivate msg = case msg of
  UndefinedTransition -> "That message does not make sense for the current state."
  PlayerNameTaken -> "That name is already taken."
  NoSuchPlayer -> "That player does not exist!"
  PlayerRoleIs role -> "The role of the player is: " ++ show role
  DoctorTargetSuccess -> "Player has been selected."
  InvestigationLimit -> "You can not investigate anymore this night."
  AlreadyLynched -> "A player has already been lynched today, no more voting this day."

encodePublic :: Public -> String
encodePublic msg = case msg of
  PlayerAdded -> "Player has been added."
  GameStarted -> "The game has been started. Good luck."
  NeedMorePlayers -> "You need to be at least 4 to start playing the game!"
  EndOfNight -> "The city now wakes up ..."
  EndOfDay -> "The city now goes to sleep ..."
  MaffiaWins -> "The maffia has won the game!"
  TownWins -> "The tow has won the game!"
  VoteCast -> "Your vote has been cast."
  VoteCancelled -> "Your vote has been cancelled."
  PlayerDied name -> "Player " ++ name ++ " has died."
  PlayerLynched name -> "Player " ++ name ++ " will be lynched."


encodeMaffia :: Maffia -> String
encodeMaffia msg = case msg of
  MaffiaTargetSuccess -> "Player has been selected."
