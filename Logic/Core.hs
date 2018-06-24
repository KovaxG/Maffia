module Core (
  run
) where

import States
import Events
import Response
import Common

import Data.List

run :: State -> Event -> (State, Response)
run originalState@(InitialState names) (AddPlayer newName)
  | playerNameExists =
    let state = originalState
        response = PlayerNameTaken
    in (state, response)
  | otherwise =
    let state = InitialState { names = newName : names }
        response = PlayerAdded
    in (state, response)
  where
    playerNameExists = elem newName names

run originalState@(InitialState names) (StartGame salt)
  | length names < 5 =
    let state = originalState
        response = NeedMorePlayers
    in (state, response)
  | otherwise =
    let state = Day { players = giveRole names }
        response = GameStarted
    in (state, response)
  where
    giveRole :: [PlayerName] -> [Player]
    giveRole newPlayers = (\(name, role) -> Player name role []) <$> nameAndRoles
      where
        nameAndRoles = zip newPlayers roles
        roles = generateRoles salt (length names)

run (Day players) EndDay =
  let state = Night {players = players}
      response = EndOfDay
  in (state, response)

run (Night players) EndNight
  | maffiaWin processedPlayers = (EndOfGame, MaffiaWins)
  | otherwise =
    let state = Day { players = processedPlayers }
        response = EndOfNight
    in (state, response)
  where
    -- should replace with a function apply effects when medics are added
    killPlayers = filterNot (\p -> elem MaffiaTarget $ pEffect p)
    processedPlayers = killPlayers players

run state (QueryRole name)
  | gameHasStarted state = maybe playerNotFound playerFound selectedPlayer
  | otherwise = (state, UndefinedTransition)
  where
    selectedPlayer = findPlayerByName name (players state)
    playerNotFound = (state, NoSuchPlayer)
    playerFound player = (state, PlayerRoleIs (show $ pRole player))

run state@(Night players) (MaffiaHit name) =
  maybe playerNotFound playerFound selectedPlayer
  where
    selectedPlayer = findPlayerByName name players
    playerNotFound = (state, NoSuchPlayer)
    playerFound player =
      let playersWithNoMaffiaHits = removeMaffiaHits <$> players
          hitPlayer = player { pEffect = MaffiaTarget : pEffect player }
          newState = state {
            players = replacePlayer hitPlayer playersWithNoMaffiaHits
          }
      in (newState, MaffiaTargetSuccess)

run state _ = (state, UndefinedTransition)
