{- Maffia Client
  All ths part needs to do, is send messages to the
  server, and the server should respond accordingly
  to each message sent.
-}

module Client where

import Common

import Network.Simple.TCP
import Control.Monad.Catch

main :: IO ()
main = do
  displayGreeting
  connectToServer $ logWith Printer

connectToServer :: Logger -> IO ()
connectToServer log = do
  address <- askForAddress
  port <- askForPort
  catch (establishConnection log address port) couldNotConnect
  where
    couldNotConnect (SomeException _) = do
      log "Could not connect."
      connectToServer log


displayGreeting :: IO ()
displayGreeting = putStrLn "Welcome to maffia by Gyuri"

askForPort :: IO String
askForPort = return "8080"

askForAddress :: IO String
askForAddress = return "localhost"

establishConnection :: Logger -> String -> String -> IO ()
establishConnection log address port =
  connect address port $ \(serverSocket, sockaddr) -> do
    log "Succesfully connected to Server!"
    sendTo serverSocket "wassup"
    handleReceive serverSocket noMessage $ messageReceived serverSocket
    where
      noMessage = log "Disconnected"

      messageReceived socket msg = do
        log msg
        loop log socket

loop :: Logger -> Socket -> IO ()
loop log socket = do
  putStr "> "
  message <- getLine

  if message == "exit"
  then putStrLn "Stopping program."
  else do
    log $ "Sending Message: " ++ message
    sendTo socket message
    handleReceive socket noAck acked
  where
    noAck = log "Disconnected from server."
    acked message = do
      log $ "Received: " ++ message
      loop log socket
