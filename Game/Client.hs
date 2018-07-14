{- Maffia Client
  All ths part needs to do, is send messages to the
  server, and the server should respond accordingly
  to each message sent.
-}

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


displayGreeting :: IO ()
displayGreeting = putStrLn "Welcome to maffia by Gyuri"

askForPort :: IO String
askForPort = return "8080"

askForAddress :: IO String
askForAddress = return "localhost"

establishConnection address port =
  connect address port $ \(serverSocket, sockaddr) -> do
    putStrLn "Succesfully connected to Server!"
    send serverSocket $ pack "wassup"
    loop serverSocket

loop :: Socket -> IO ()
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
    acked message = do
      putStrLn $ unpack message
      loop socket
