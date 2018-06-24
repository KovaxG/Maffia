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
runTests = mapM_ putStrLn testOutputs
  where testOutputs = formatTestOutputs (runTest <$> stateTests)

stateTests = [
  Test "Adding new player"
  (run (InitialState []) (AddPlayer "Gyuri"))
  (InitialState ["Gyuri"], PlayerAdded),

  Test "Cannot add players with same name"
  (run (InitialState ["Gyuri"]) (AddPlayer "Gyuri"))
  (InitialState ["Gyuri"], PlayerNameTaken),

  Test "Cannot start game if there are less then 5 players"
  (run (InitialState ["Gyuri"]) (StartGame 5))
  (InitialState ["Gyuri"], NeedMorePlayers),

  Test "Can start game if there are at least 5 players"
  (run (InitialState playerNames) (StartGame 5))
  (Day playerList, GameStarted),

  Test "Player can ask for role in day"
  (run (Day playerList) (QueryRole "Gyuri"))
  (Day playerList, PlayerRoleIs "Civilian"),

  Test "Player can ask for role in night"
  (run (Night playerList) (QueryRole "Gyuri"))
  (Night playerList, PlayerRoleIs "Civilian"),

  Test "Not existing player role"
  (run (Day playerList) (QueryRole "Simpson"))
  (Day playerList, NoSuchPlayer),

  Test "Asking for role before game starting"
  (run (InitialState ["Gyuri"]) (QueryRole "Gyuri"))
  (InitialState ["Gyuri"], UndefinedTransition),

  Test "Maffia can not hit during the day"
  (run (Day playerList) (MaffiaHit "Gyuri"))
  (Day playerList, UndefinedTransition),

  Test "Maffia hits on one player"
  (run (Night [Player "Gyuri" Civilian []]) (MaffiaHit "Gyuri"))
  ((Night [Player "Gyuri" Civilian [MaffiaTarget]]), MaffiaTargetSuccess),

  Test "Maffia hit on non existant player"
  (run (Night [Player "Gyuri" Civilian []]) (MaffiaHit "Masvalaki"))
  ((Night [Player "Gyuri" Civilian []]), NoSuchPlayer),

  Test "Changing the maffia targeted player results in a single targeted player"
  (run (Night [Player "Gyuri" Civilian [MaffiaTarget], Player "Tamas" Civilian []]) (MaffiaHit "Tamas"))
  ((Night [Player "Tamas" Civilian [MaffiaTarget], Player "Gyuri" Civilian []]), MaffiaTargetSuccess),

    Test "Player with maffia hit dies when day comes"
    (run (Night playerHit) EndNight)
    ((Day playerHitDies), EndOfNight),

    Test "If nr of maffia is greater or equal to civilians, maffia win"
    (run (Night maffiaAlmostWinPlayersList) EndNight)
    (EndOfGame, MaffiaWins)
  ]

playerNames = ["Gyuri", "Tamas", "Kristof", "Geza", "Zoli", "Robert"]
playerList = toPlayer <$> playerNames
  where toPlayer name
          | name == "Tamas" || name == "Kristof" = Player name Maffia []
          | otherwise = Player name Civilian []

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

main :: IO ()
main = gameLoop $ InitialState []
  where
    gameLoop :: State -> IO ()
    gameLoop state = do
      putStr "> "
      input <- read <$> getLine :: IO Event
      let (nextState, response) = run state input
      putStrLn $ toMessage response
      if nextState == EndOfGame
      then return ()
      else gameLoop nextState
