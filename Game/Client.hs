-- Maffia Client

import Network.Simple.TCP
import Data.ByteString.Char8 (pack, unpack)
import Control.Monad.Catch

main :: IO ()
main = displayGreeting >> connectToServer

connectToServer :: IO ()
connectToServer = do
  address <- askForAddress
  port <- askForPort
  catch (establishConnection address port) couldNotConnect
  where
    couldNotConnect (SomeException _) = putStrLn "Could not connect." >> connectToServer
    establishConnection address port =
      connect address port $ \(serverSocket, sockaddr) -> do
        putStrLn "Succesfully connected to Server!"
        loop serverSocket


displayGreeting :: IO ()
displayGreeting = putStrLn "Welcome to maffia by Gyuri"

askForPort :: IO String
askForPort = putStr "Port: " >> getLine

askForAddress :: IO String
askForAddress = putStr "IP:   " >> getLine

loop socket = do
  putStr "> "
  message <- getLine

  if message == "exit"
  then putStrLn "Stopping program."
  else do
    send socket $ pack message
    ack <- recv socket 256
    maybe noAck acked ack
  where
    noAck = putStrLn "Disconnected from server."
    acked _ = loop socket
