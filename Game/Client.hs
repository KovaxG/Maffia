module Client where

import Network.Simple.TCP
import Data.ByteString.Char8 (pack, unpack)

main = do 
    putStrLn "Connecting to server."
    connect "localhost" "8080" $ \(serverSocket, sockaddr) -> do
        putStrLn $ "Succesfully connected to " ++ show sockaddr
        putStrLn "Write any message, send \"exit\" to disconnect."
        loop serverSocket


loop socket = do
        putStr "> "
        message <- getLine
        
        if message == "exit"
        then putStrLn "Exited"
        else do
            send socket $ pack message
            response <- recv socket 2048
            maybe noAck acked response
            where 
                noAck = putStrLn "No Message."
                acked r = do
                    putStrLn $ unpack r
                    loop socket