-- Simple Client Example

import Network.Simple.TCP
import Data.ByteString.Char8 (pack, unpack)

main = do
    putStrLn "Connecting to server."
    connect "localhost" "8080" $ \(serverSocket, sockaddr) -> do
        putStrLn $ "Succesfully connected to " ++ show sockaddr
        loop serverSocket


loop socket = do
  msg <- recv socket 256
  maybe nothing ok msg
  loop socket
  where
    ok msg = putStrLn $ unpack msg
    nothing = putStrLn "Disconnected"
