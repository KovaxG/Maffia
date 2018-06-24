module Core (
  run
) where

import States
import Events
import Response
import Common

import Data.List
import Data.Maybe

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
  | length names < 4 =
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
    processedPlayers = applyEffects players

run state (QueryRole name)
  | gameHasStarted state = maybe playerNotFound playerFound selectedPlayer
  | otherwise = (state, UndefinedTransition)
  where
    selectedPlayer = findPlayerByName name (players state)
    playerNotFound = (state, NoSuchPlayer)
    playerFound player = (state, PlayerRoleIs (pRole player))

run state@(Night players) (MaffiaHit name) =
  maybe playerNotFound playerFound selectedPlayer
  where
    selectedPlayer = findPlayerByName name players
    playerNotFound = (state, NoSuchPlayer)
    playerFound player =
      let playersWithNoMaffiaHits =
            removeEffect MaffiaTarget <$> players
          hitPlayer = player {
            pEffects = MaffiaTarget : pEffects player
          }
          newState = state {
            players = replacePlayer hitPlayer playersWithNoMaffiaHits
          }
      in (newState, MaffiaTargetSuccess)

-- should merge MaffiaHit with DoctorSave
run state@(Night players) (DoctorSave name) =
  maybe playerNotFound playerFound selectedPlayer
  where
    selectedPlayer = findPlayerByName name players
    playerNotFound = (state, NoSuchPlayer)
    playerFound player =
      let playersWithNoDoctorSaves =
            removeEffect DoctorTarget <$> players
          savedPlayer = player {
            pEffects = DoctorTarget : pEffects player
          }
          newState = state {
            players = replacePlayer savedPlayer playersWithNoDoctorSaves
          }
      in (newState, DoctorTargetSuccess)

run state@(Night players) (Investigate name) =
  maybe playerNotFound playerFound selectedPlayer
  where
    selectedPlayer = findPlayerByName name players
    playerNotFound = (state, NoSuchPlayer)
    playerFound player
      | alreadyInvestigated = (state, InvestigationLimit)
      | otherwise =
        let investigatedPlayer = player {
              pEffects = Investigated : pEffects player
            }
            newState = state {
              players = replacePlayer investigatedPlayer players
            }
        in (newState, PlayerRoleIs $ pRole player)
      where
        alreadyInvestigated = elem Investigated (pEffects =<< players)

run state@(Day players) (Vote voterName votedName) =
  maybe playerNotFound id $ performVote <$> voter <*> voted
  where
    voted = findPlayerByName votedName players
    voter = findPlayerByName voterName players
    playerNotFound = (state, NoSuchPlayer)

    -- TODO this logic is pretty crappy and convoluted
    performVote :: Player -> Player -> (State, Response)
    performVote voter voted
      | votedHasMajorityVote =
        let lynchedPerson = voted { pEffects = [Lynched] }
            newState = state {
              players = replacePlayer lynchedPerson players
            }
        in (newState, VoteCast)
      | otherwise =
        let playersWithNoVote = removeEffect vote <$> players
            votedWithVote = addEffect vote voted
            newPlayers = replacePlayer votedWithVote playersWithNoVote
            newState = state { players = newPlayers }
        in (newState, VoteCast)
      where
        vote = VotedBy $ pName voter
        votedHasMajorityVote = countVotes > (length players `div` 2)
        -- not the best approach, since we count any effects
        countVotes = (length $ pEffects voted) + 1

run state@(Day players) (CancelVote name) =
  let playersWithoutVote = removeEffect vote <$> players
      newState = state { players = playersWithoutVote }
  in (newState, VoteCancelled)
  where
    vote = VotedBy name

run state _ = (state, UndefinedTransition)

applyEffect :: Player -> Maybe Player
applyEffect player
  | pEffects player == [MaffiaTarget] = Nothing
  | otherwise = Just $ player { pEffects = [] }

applyEffects :: [Player] -> [Player]
applyEffects players =
  map fromJust $
  filter isJust $
  applyEffect <$> players
