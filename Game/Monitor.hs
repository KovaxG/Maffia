{-  Monitor
 This window serves as just a window to see the
 messages from other players and from the server.
-}

module Monitor where

import Common

import Network.Simple.TCP

main :: IO ()
main = do
    putStrLn "Connecting to server."
    let ip = "localhost"
    let port = 8080
    let logger = logWith Printer
    connect ip (show port) (monitorLogic logger)

monitorLogic :: Logger -> (Socket, SockAddr) -> IO ()
monitorLogic log (serverSocket, sockaddr) = do
  log $ "Succesfully connected to " ++ show sockaddr
  sendTo serverSocket "Monitor"
  loop log serverSocket

loop :: Logger -> Socket -> IO ()
loop log socket = do
  handleReceive socket noResponse response
  where
    noResponse = log "Disconnected"
    response msg = do
      log msg
      loop log socket
