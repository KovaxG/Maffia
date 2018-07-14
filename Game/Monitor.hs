{-  Monitor
 This window serves as just a window to see the
 messages from other players and from the server.
-}

module Monitor where

import Network.Simple.TCP
import Data.ByteString.Char8 (pack, unpack)

main :: IO ()
main = do
    putStrLn "Connecting to server."
    let ip = "localhost"
    let port = 8080
    connect ip (show port) monitorLogic

monitorLogic :: (Socket, SockAddr) -> IO ()
monitorLogic (serverSocket, sockaddr) = do
  putStrLn $ "Succesfully connected to " ++ show sockaddr
  send serverSocket $ pack "Monitor"
  loop serverSocket

loop :: Socket -> IO ()
loop socket = do
  message <- recv socket 2048
  maybe noResponse response message
  where
    noResponse = putStrLn "Disconnected"
    response byteString = do
      let msg = unpack byteString
      putStrLn msg
      loop socket
