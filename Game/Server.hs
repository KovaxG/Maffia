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

  handleReceive socket noResponse $ response socket broadcastList gameState
  where
    noResponse = log "Disconnected"

    response :: Socket -> MVar [Socket] -> MVar State -> String -> IO ()
    response socket broadcastList gameState response = case response of
      "Monitor" -> do
        log "Monitor Connected"
        addMonitor socket broadcastList
        sleepForever
      _ -> do
        log $ getName socket ++ "Client connected"
        socks <- readMVar broadcastList
        log $ show socks
        loop log socket broadcastList gameState

addMonitor :: Socket -> MVar [Socket] -> IO ()
addMonitor socket socketsVar = do
  sockets <- takeMVar socketsVar
  putMVar socketsVar $ socket : sockets
  putStrLn $ show (socket : sockets)
  putStrLn "Montior added to list"

loop :: Logger -> Socket -> MVar [Socket] -> MVar State -> IO ()
loop log socket broadcastList gameState =
  handleReceive socket noMessage $ messageReceived broadcastList
  where
    noMessage = putStrLn "Disconnected"
    messageReceived :: MVar [Socket] -> String -> IO ()
    messageReceived broadcastList msg = do
      let eventMaybe = safeRead $ drop 6 message :: Maybe Event
      maybe failedParsing successParsing eventMaybe
      sendTo socket "ok"

      loop log socket broadcastList gameState
      where
        message = getName socket ++ msg

        failedParsing = do
          log message
          broadCastMessages broadcastList message

        successParsing event = do
          state <- takeMVar gameState
          let (newState, responses) = run state event
          traverse processResponse responses
          putMVar gameState newState
          log $ show newState
          where
            processResponse :: Response -> IO ()
            processResponse (BroadCast msg) = do
              log $ encodePublic msg
              broadCastMessages broadcastList $ encodePublic msg
            processResponse _ = return ()

broadCastMessages :: MVar [Socket] -> String -> IO ()
broadCastMessages broadcastList msg = do
  sockets <- readMVar broadcastList
  trace ("socks: " ++ show sockets) (return ())
  mapM_ (\s -> sendTo s msg) sockets

getName :: Socket -> String
getName socket = "[" ++ name' ++ "] "
  where name' = init $ drop 9 $ show socket
