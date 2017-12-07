-- Monitor

module Monitor where

import Network.Simple.TCP
import Data.ByteString.Char8 (pack, unpack)

main = do 
    putStrLn "Connecting to server."
    connect "localhost" "8080" $ \(serverSocket, sockaddr) -> do
        putStrLn $ "Succesfully connected to " ++ show sockaddr
        send serverSocket $ pack "monitor"
        loop serverSocket


loop socket = do
        message <- recv socket 2048
        maybe noResponse response message
        where 
            noResponse = do
                putStrLn "Exited"
            response byteString = do
                let msg = unpack byteString
                putStrLn msg
                loop socket