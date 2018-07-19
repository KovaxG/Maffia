module Server where

import Common
import Logic.Core
import Logic.States
import Logic.Common (safeRead)
import Logic.Events
import Logic.Response

import Network.Simple.TCP
import Control.Concurrent.MVar
import Text.Read (readMaybe)
import Data.List (find)
import Debug.Trace

-- TODO try to add better logs, cause these ones are just horrid

data Player = Player {
  socketOf :: Socket,
  nameOf :: String
}

main :: IO ()
main = do
  broadcastList <- newMVar []
  gameState <- newMVar (InitialState [])
  log "Listening for connections."
  serve host port $ startServer log broadcastList gameState
  where
    host = Host "localhost"
    port = "8080"
    log = logWith Printer

startServer :: Logger -> MVar [Socket] -> MVar State -> (Socket, SockAddr) -> IO ()
startServer log broadcastList gameState (socket, remoteAddr) = do
  log $ "Connected to " ++ show remoteAddr
  handleReceive socket noResponse $ response socket broadcastList gameState
  where
    noResponse = log "No response."

    response :: Socket -> MVar [Socket] -> MVar State -> String -> IO ()
    response socket broadcastList gameState response = case response of
      "Monitor" -> monitorConnected log socket broadcastList
      _ -> do
        maybePlayer <- handleClientName log socket broadcastList gameState
        maybe couldNotConnect connected maybePlayer
        where
          couldNotConnect = log "Failed to connect player"
          connected player = clientConnected log player broadcastList gameState

-- TODO I always thandle the noMessage the same way, and always send
-- to the same socket. One ought to partially apply that crap
handleClientName :: Logger -> Socket -> MVar [Socket] -> MVar State -> IO (Maybe Player)
handleClientName log socket broadcastList gameState = do
  askForName
  handleReceive socket noMessage nameReceived
  where
    askForName :: IO ()
    askForName = sendTo socket "Please choose a name."

    noMessage = do
      log "Failed to get player name."
      return Nothing

    nameReceived name = do
      responses <- processEvent log gameState $ AddPlayer name
      mapM_ (processResponse log broadcastList) responses
      if hasPlayerBeenAdded responses
      then do
        sendTo socket "You have been added to the game."
        log $ "Player has been added to the game. " ++ name
        return $ Just $ Player socket ("[" ++ name ++ "]")
      else handleClientName log socket broadcastList gameState

    hasPlayerBeenAdded :: [Response] -> Bool
    hasPlayerBeenAdded responses = elem (BroadCast PlayerAdded) responses

processEvent :: Logger -> MVar State -> Event -> IO [Response]
processEvent log gameState event = do
  state <- takeMVar gameState
  let (newState, responses) = run state event
  putMVar gameState newState
  log $ show newState
  return responses

monitorConnected :: Logger -> Socket -> MVar [Socket] -> IO ()
monitorConnected log socket broadcastList = do
  log "Monitor Connected"
  addMonitor socket broadcastList
  sleepForever

addMonitor :: Socket -> MVar [Socket] -> IO ()
addMonitor socket socketsVar = do
  sockets <- takeMVar socketsVar
  putMVar socketsVar $ socket : sockets
  putStrLn $ show (socket : sockets)
  putStrLn "Montior added to list"

clientConnected :: Logger -> Player -> MVar [Socket] -> MVar State -> IO ()
clientConnected log player broadcastList gameState = do
  log $ nameOf player ++ "Client connected"
  socks <- readMVar broadcastList
  log $ show socks
  loop log player broadcastList gameState

loop :: Logger -> Player -> MVar [Socket] -> MVar State -> IO ()
loop log player broadcastList gameState =
  handleReceive (socketOf player) noMessage $ messageReceived broadcastList
  where
    noMessage = putStrLn "Disconnected"

    messageReceived :: MVar [Socket] -> String -> IO ()
    messageReceived broadcastList msg = do
      let eventMaybe = safeRead msg :: Maybe Event
      response <- maybe failedParsing successParsing eventMaybe
      sendTo (socketOf player) response

      loop log player broadcastList gameState
      where
        message = nameOf player ++ " " ++ msg

        failedParsing = do
          log message
          broadCastMessages broadcastList message
          return "ok"

        successParsing event = do
          state <- takeMVar gameState
          let (newState, responses) = run state event
          putMVar gameState newState
          traverse (processResponse log broadcastList) responses
          log $ show newState
          return "ok" -- need to read this message from the responses

processResponse :: Logger -> MVar [Socket] -> Response -> IO ()
processResponse log broadcastList (BroadCast msg) = do
  log $ encodePublic msg
  broadCastMessages broadcastList $ encodePublic msg
processResponse _ _ _ = return ()

broadCastMessages :: MVar [Socket] -> String -> IO ()
broadCastMessages broadcastList msg = do
  sockets <- readMVar broadcastList
  trace ("socks: " ++ show sockets) (return ())
  mapM_ (\s -> sendTo s msg) sockets

getName :: Socket -> String
getName socket = "[" ++ name' ++ "] "
  where name' = init $ drop 9 $ show socket
