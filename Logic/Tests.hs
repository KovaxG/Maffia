module Tests where

import Core
import Common
import States
import Response
import Events

-- Test name expression expected
data Test a = Test TestName a a
type TestName = String

runTest :: (Eq a, Show a) => Test a -> (String, Bool)
runTest (Test name expression expected) =
  if expression == expected
  then (testPassedMsg, True)
  else (testFailedMsg, False)
  where testPassedMsg = name ++ " test passed."
        testFailedMsg = name ++ " failed!\nExpected: " ++ show expected ++ "\n  Actual: " ++ show expression


formatTestOutputs :: [(String, Bool)] -> [String]
formatTestOutputs outputs = applyRule <$> zip outputs [1 ..]
  where applyRule :: ((String, Bool), Int) -> String
        applyRule ((msg, success), index)
          | success == False = "\n" ++ show index ++ ". " ++ msg ++ "\n"
          | otherwise        = show index ++ ". " ++ msg

runTests :: IO ()
runTests = do
  mapM_ putStrLn (formatTestOutputs testOutputs)
  putStrLn $ "Passed: " ++ show nrPassed
  putStrLn $ "Failed: " ++ show nrFailed
  where
    testOutputs = runTest <$> stateTests
    nrPassed = count snd testOutputs
    nrFailed = (length testOutputs) - nrPassed

stateTests = [
  Test "Adding new player"
  (run (InitialState []) (AddPlayer "Gyuri"))
  (InitialState ["Gyuri"], [BroadCast PlayerAdded]),

  Test "Cannot add players with same name"
  (run (InitialState ["Gyuri"]) (AddPlayer "Gyuri"))
  (InitialState ["Gyuri"], [Feedback PlayerNameTaken]),

  Test "Cannot start game if there are less then 4 players"
  (run (InitialState (tail playerNames)) (StartGame 5))
  (InitialState (tail playerNames), [BroadCast NeedMorePlayers]),

  Test "Can start game if there are at least 4 players"
  (run (InitialState playerNames) (StartGame 5))
  (Day playerList, [BroadCast GameStarted]),

  Test "Player can ask for role in day"
  (run (Day playerList) (QueryRole "Gyuri"))
  (Day playerList, [Feedback (PlayerRoleIs Maffia)]),

  Test "Player can ask for role in night"
  (run (Night playerList) (QueryRole "Gyuri"))
  (Night playerList, [Feedback (PlayerRoleIs Maffia)]),

  Test "Not existing player role"
  (run (Day playerList) (QueryRole "Simpson"))
  (Day playerList, [Feedback NoSuchPlayer]),

  Test "Asking for role before game starting"
  (run (InitialState ["Gyuri"]) (QueryRole "Gyuri"))
  (InitialState ["Gyuri"], [Feedback UndefinedTransition]),

  Test "Maffia can not hit during the day"
  (run (Day playerList) (MaffiaHit "Gyuri"))
  (Day playerList, [Feedback UndefinedTransition]),

  Test "Maffia hits on one player"
  (run (Night [Player "Gyuri" Civilian []]) (MaffiaHit "Gyuri"))
  ((Night [Player "Gyuri" Civilian [MaffiaTarget]]), [TeamMessage MaffiaTargetSuccess]),

  Test "Maffia hit on non existant player"
  (run (Night [Player "Gyuri" Civilian []]) (MaffiaHit "Masvalaki"))
  ((Night [Player "Gyuri" Civilian []]), [Feedback NoSuchPlayer]),

  Test "Changing the maffia targeted player results in a single targeted player"
  (run (Night [Player "Gyuri" Civilian [MaffiaTarget], Player "Tamas" Civilian []]) (MaffiaHit "Tamas"))
  ((Night [Player "Tamas" Civilian [MaffiaTarget], Player "Gyuri" Civilian []]), [TeamMessage MaffiaTargetSuccess]),

  Test "Player with maffia hit dies when day comes"
  (run (Night playerHit) EndNight)
  ((Day playerHitDies), [BroadCast EndOfNight, BroadCast (PlayerDied "Zoli")]),

  Test "If nr of maffia is greater or equal to civilians, maffia win"
  (run (Night maffiaAlmostWinPlayersList) EndNight)
  (EndOfGame, [BroadCast MaffiaWins, BroadCast (PlayerDied "Kristof")]),

  Test "Doctor hits on one player"
  (run (Night [Player "Gyuri" Civilian []]) (DoctorSave "Gyuri"))
  (Night [Player "Gyuri" Civilian [DoctorTarget]], [Feedback DoctorTargetSuccess]),

  Test "Doctor hits on non existent player"
  (run (Night []) (DoctorSave "Gyuri"))
  (Night [], [Feedback NoSuchPlayer]),

  Test "Player with maffia and doctor target lives the next day"
  (run (Night playersWithDoctorAndMaffiaHit) EndNight)
  (Day playerList, [BroadCast EndOfNight]),

  Test "Detective can investigate a player"
  (run (Night [Player "Gyuri" Civilian []]) (Investigate "Gyuri"))
  (Night [Player "Gyuri" Civilian [Investigated]], [Feedback (PlayerRoleIs Civilian)]),

  Test "Detective can not investigate non existant player"
  (run (Night [Player "Gyuri" Civilian []]) (Investigate "Luri"))
  (Night [Player "Gyuri" Civilian []], [Feedback NoSuchPlayer]),

  Test "Detective can not investigate in the day"
  (run  (Day [Player "Gyuri" Civilian []]) (Investigate "Gyuri"))
  (Day [Player "Gyuri" Civilian []], [Feedback UndefinedTransition]),

  Test "Detective can only investiage once a turn"
  (run (Night playersInvestigated) (Investigate "Gyuri"))
  (Night playersInvestigated, [Feedback InvestigationLimit]),

  Test "You can not vote during the night"
  (run (Night playerList) (Vote "Gyuri" "Kristof"))
  (Night playerList, [Feedback UndefinedTransition]),

  Test "Voter does not exist"
  (run (Day playerList) (Vote "Hans" "Kristof"))
  (Day playerList, [Feedback NoSuchPlayer]),

  Test "Voted person does not exist"
  (run (Day playerList) (Vote "Kristof" "Hans"))
  (Day playerList, [Feedback NoSuchPlayer]),

  Test "Successfull vote"
  (run (Day preVotePlayers) (Vote "Kristof" "Gyuri"))
  (Day postVotePlayers, [BroadCast VoteCast]),

  Test "Voter cancels his vote"
  (run (Day postVotePlayers) (CancelVote "Kristof"))
  (Day preVotePlayers, [BroadCast VoteCancelled]),

  Test "Voter changes his vote"
  (run (Day postVotePlayers) (Vote "Kristof" "Tamas"))
  (Day postVoteChanged, [BroadCast VoteCast]),

  Test "Majority vote for lynching, player will be lynched"
  (run (Day almostMajorityVotes) (Vote "Geza" "Tamas"))
  (Day postMajorityVotes, [BroadCast VoteCast, BroadCast (PlayerLynched "Tamas")]),

  Test "Players can not vote after a player has been lynched"
  (run (Day postMajorityVotes) (Vote "Geza" "Gyuri"))
  (Day postMajorityVotes, [Feedback AlreadyLynched]),

  Test "Lynched players are removed at end of day"
  (run (Day postMajorityVotes) EndDay)
  (Night postMajorityVotesRemoved, [BroadCast EndOfDay]),

  Test "If last maffia is lynched town wins"
  (run (Day lastMaffiaLynched) EndDay)
  (EndOfGame, [BroadCast TownWins]),

  Test "If last maffia is killed by maffia, town wins"
  (run (Night maffiaSuicide) EndNight)
  (EndOfGame, [BroadCast (PlayerDied "Gyuri"), BroadCast TownWins])
  ]

playerNames = pName <$> playerList
playerList = [
  Player "Gyuri" Maffia [],
  Player "Tamas" Doctor [],
  Player "Kristof" Detective [],
  Player "Geza" Civilian []
  ]

playersWithDoctorAndMaffiaHit = [
  Player "Gyuri" Maffia [],
  Player "Tamas" Doctor [],
  Player "Kristof" Detective [MaffiaTarget, DoctorTarget],
  Player "Geza" Civilian []
  ]

playerHit = [
  Player "Gyuri" Maffia [],
  Player "Tamas" Civilian [],
  Player "Zoli" Civilian [MaffiaTarget],
  Player "Brigi" Civilian []
  ]

playerHitDies = [
    Player "Gyuri" Maffia [],
    Player "Tamas" Civilian [],
    Player "Brigi" Civilian []
    ]

maffiaAlmostWinPlayersList = [
  Player "Gyuri" Maffia [],
  Player "Tamas" Maffia [],
  Player "Zoli" Civilian [],
  Player "Kristof" Civilian [MaffiaTarget],
  Player "Imelda" Civilian []
  ]

playersInvestigated = [
  Player "Gyuri" Maffia [Investigated],
  Player "Tamas" Detective [],
  Player "Kristof" Doctor [],
  Player "Geza" Civilian []
  ]

preVotePlayers = [
  Player "Gyuri" Maffia [],
  Player "Tamas" Detective [],
  Player "Kristof" Doctor [],
  Player "Geza" Civilian []
  ]

postVotePlayers = [
  Player "Gyuri" Maffia [VotedBy "Kristof"],
  Player "Tamas" Detective [],
  Player "Kristof" Doctor [],
  Player "Geza" Civilian []
  ]

postVoteChanged = [
  Player "Tamas" Detective [VotedBy "Kristof"],
  Player "Gyuri" Maffia [],
  Player "Kristof" Doctor [],
  Player "Geza" Civilian []
  ]

almostMajorityVotes = [
  Player "Gyuri" Maffia [],
  Player "Tamas" Detective [VotedBy "Kristof", VotedBy "Gyuri"],
  Player "Kristof" Doctor [],
  Player "Geza" Civilian []
  ]

postMajorityVotes = [
  Player "Tamas" Detective [Lynched],
  Player "Gyuri" Maffia [],
  Player "Kristof" Doctor [],
  Player "Geza" Civilian []
  ]

postMajorityVotesRemoved = [
  Player "Gyuri" Maffia [],
  Player "Kristof" Doctor [],
  Player "Geza" Civilian []
  ]

lastMaffiaLynched = [
  Player "Gyuri" Maffia [Lynched],
  Player "Kristof" Doctor [],
  Player "Geza" Civilian []
  ]

maffiaSuicide = [
  Player "Gyuri" Maffia [MaffiaTarget],
  Player "Kristof" Doctor [],
  Player "Geza" Civilian []
  ]

startingState = InitialState []

runGame :: State -> [Event] -> [Response]
runGame startState events =
  snd $ foldl rule (InitialState [], []) events
  where
    rule :: (State, [Response]) -> Event -> (State, [Response])
    rule (state, responses) event =
      let (newState, response) = run state event
      in (newState, responses ++ response)

main :: IO ()
main = do
  gameLoop $ InitialState []
  where
    gameLoop :: State -> IO ()
    gameLoop state = do
      putStr "> "
      input <- safeRead <$> getLine :: IO (Maybe Event)
      maybe (incorrectInput state) (correctInput state) input

    incorrectInput state = do
      putStrLn "Parse Error!"
      gameLoop state

    correctInput state input = do
      let (nextState, responses) = run state input
      mapM_ putStrLn $ toMessage <$> responses
      if nextState == EndOfGame
      then return ()
      else gameLoop nextState
