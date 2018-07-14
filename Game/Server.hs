module Server where

import Network.Simple.TCP
import Data.ByteString.Char8 (pack, unpack)
import Control.Concurrent.MVar
import Text.Read (readMaybe)
import Data.List (find)
import Debug.Trace
import Control.Concurrent.Thread.Delay

initialState :: [Socket]
initialState = []

main :: IO ()
main = do
  gameState <- newMVar initialState
  broadCastList <- newMVar []

  putStrLn "Listening for connections."
  serve (Host "localhost") "8080" $ \(socket, remoteAddr) -> do
      role <- recv socket 100
      maybe noResponse (response socket broadCastList) $ unpack <$> role
        where
          noResponse = putStrLn "Disconnected"
          response socket broadCastList resp =
            if resp == "Monitor"
            then do
              putStrLn "Monitor Connected"
              addMonitor socket broadCastList
              sleepForever
            else do
              putStrLn "Client connected"
              loop socket broadCastList

sleepForever :: IO ()
sleepForever = do
  delay 100000000000000000
  sleepForever

addMonitor :: Socket -> MVar [Socket] -> IO ()
addMonitor socket socketsVar = do
  sockets <- takeMVar socketsVar
  putMVar socketsVar $ socket : sockets
  putStrLn "Montior added to list"

loop :: Socket -> MVar [Socket] -> IO ()
loop socket broadCastList = do
  msg <- recv socket 1024
  maybe noMessage (message broadCastList) $ unpack <$> msg
  where
    noMessage = putStrLn "Disconnected"
    message broadCastList msg = do
      putStrLn msg
      send socket $ pack "ok"
      broadCastMessages broadCastList msg
      loop socket broadCastList

broadCastMessages :: MVar [Socket] -> String -> IO ()
broadCastMessages broadCastList msg = do
  sockets <- readMVar broadCastList
  mapM_ (\s -> send s $ pack msg) sockets
