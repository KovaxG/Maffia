module Logic.Core (
  run
) where

import Logic.States
import Logic.Events
import Logic.Response
import Logic.Common

import Data.List
import Data.Maybe

run :: State -> Event -> (State, [Response])
run originalState@(InitialState names) (AddPlayer newName)
  | playerNameExists =
    let responses = [Feedback PlayerNameTaken]
    in (originalState, responses)
  | otherwise =
    let state = InitialState { names = newName : names }
        responses = [BroadCast PlayerAdded]
    in (state, responses)
  where
    playerNameExists = elem newName names

run originalState@(InitialState names) (StartGame salt)
  | length names < 4 =
    let responses = [BroadCast NeedMorePlayers]
    in (originalState, responses)
  | otherwise =
    let state = Day { players = giveRole names }
        responses = [BroadCast GameStarted]
    in (state, responses)
  where
    giveRole :: [PlayerName] -> [Player]
    giveRole newPlayers = zipWith toPlayer newPlayers roles
      where
        roles = generateRoles salt (length names)
        toPlayer = \name role -> Player name role []

run (Day players) EndDay
  | allMaffiaDead = (EndOfGame, [BroadCast TownWins])
  | otherwise =
    let state = Night { players = newPlayers }
        responses = [BroadCast EndOfDay]
    in (state, responses)
  where
    removeLynched = filterNot $ \p -> elem Lynched (pEffects p)
    allMaffiaDead = count isMaffia newPlayers == 0
    newPlayers = removeLynched players

run (Night players) EndNight
  | allMaffiaDead = (EndOfGame, playerDiedReport ++ [BroadCast TownWins])
  | maffiaWinGiven processedPlayers = (EndOfGame, [BroadCast MaffiaWins] ++ playerDiedReport)
  | otherwise =
    let state = Day { players = processedPlayers }
        responses = [BroadCast EndOfNight] ++ playerDiedReport
    in (state, responses)
  where
    processedPlayers = applyEffects players
    allMaffiaDead = count isMaffia processedPlayers == 0
    playersDied = players \\ processedPlayers
    playerDiedReport = (BroadCast . PlayerDied . pName) <$> playersDied

run state (QueryRole name)
  | gameHasStarted state = maybe playerNotFound playerFound selectedPlayer
  | otherwise = (state, [Feedback UndefinedTransition])
  where
    selectedPlayer = findPlayerByName name (players state)
    playerNotFound = (state, [Feedback NoSuchPlayer])
    playerFound player = (state, [Feedback (PlayerRoleIs (pRole player))])

run state@(Night players) (MaffiaHit name) =
  maybe playerNotFound playerFound selectedPlayer
  where
    selectedPlayer = findPlayerByName name players
    playerNotFound = (state, [Feedback NoSuchPlayer])
    playerFound player =
      let playersWithNoMaffiaHits =
            removeEffect MaffiaTarget <$> players
          hitPlayer = player {
            pEffects = MaffiaTarget : pEffects player
          }
          newState = state {
            players = replacePlayer hitPlayer playersWithNoMaffiaHits
          }
      in (newState, [TeamMessage MaffiaTargetSuccess])

-- should merge MaffiaHit with DoctorSave
run state@(Night players) (DoctorSave name) =
  maybe playerNotFound playerFound selectedPlayer
  where
    selectedPlayer = findPlayerByName name players
    playerNotFound = (state, [Feedback NoSuchPlayer])
    playerFound player =
      let playersWithNoDoctorSaves =
            removeEffect DoctorTarget <$> players
          savedPlayer = player {
            pEffects = DoctorTarget : pEffects player
          }
          newState = state {
            players = replacePlayer savedPlayer playersWithNoDoctorSaves
          }
      in (newState, [Feedback DoctorTargetSuccess])

run state@(Night players) (Investigate name) =
  maybe playerNotFound playerFound selectedPlayer
  where
    selectedPlayer = findPlayerByName name players
    playerNotFound = (state, [Feedback NoSuchPlayer])
    playerFound player
      | alreadyInvestigated = (state, [Feedback InvestigationLimit])
      | otherwise =
        let investigatedPlayer = player {
              pEffects = Investigated : pEffects player
            }
            newState = state {
              players = replacePlayer investigatedPlayer players
            }
            playerRole = pRole player
        in (newState, [Feedback (PlayerRoleIs playerRole)])
      where
        alreadyInvestigated = elem Investigated (pEffects =<< players)

run state@(Day players) (Vote voterName votedName)
  | someOneIsAlreadyLynched = (state, [Feedback AlreadyLynched])
  | otherwise =
    maybe playerNotFound id $ performVote <$> voter <*> voted
  where
    someOneIsAlreadyLynched = elem Lynched (pEffects =<< players)

    voted = findPlayerByName votedName players
    voter = findPlayerByName voterName players
    playerNotFound = (state, [Feedback NoSuchPlayer])

    -- TODO this logic is pretty crappy and convoluted
    performVote :: Player -> Player -> (State, [Response])
    performVote voter voted
      | votedHasMajorityVote =
        let lynchedPerson = voted { pEffects = [Lynched] }
            newState = state {
              players = replacePlayer lynchedPerson players
            }
            responses = [
              BroadCast VoteCast,
              BroadCast $ PlayerLynched $ pName lynchedPerson
              ]
        in (newState, responses)
      | otherwise =
        let playersWithNoVote = removeEffect vote <$> players
            votedWithVote = addEffect vote voted
            newPlayers = replacePlayer votedWithVote playersWithNoVote
            newState = state { players = newPlayers }
        in (newState, [BroadCast VoteCast])
      where
        vote = VotedBy $ pName voter
        votedHasMajorityVote = countVotes > (length players `div` 2)
        countVotes = 1 + (count isVote (pEffects voted))
        isVote (VotedBy _) = True
        isVote _ = False

run state@(Day players) (CancelVote name) =
  let playersWithoutVote = removeEffect vote <$> players
      newState = state { players = playersWithoutVote }
  in (newState, [BroadCast VoteCancelled])
  where
    vote = VotedBy name

run state _ = (state, [Feedback UndefinedTransition])

applyEffect :: Player -> Maybe Player
applyEffect player
  | pEffects player == [MaffiaTarget] = Nothing
  | otherwise = Just $ player { pEffects = [] }

applyEffects :: [Player] -> [Player]
applyEffects players =
  map fromJust $
  filter isJust $
  applyEffect <$> players
