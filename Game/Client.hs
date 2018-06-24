module Client where

import Network.Simple.TCP
import Data.ByteString.Char8 (pack, unpack)

main :: IO ()
main = do 
    putStrLn "Connecting to server."
    connect "localhost" "8080" $ \(serverSocket, sockaddr) -> do
        putStrLn "Write any message, send \"exit\" to disconnect."
        mainLoop serverSocket


mainLoop :: Socket -> IO ()
mainLoop socket = do
        putStr "> "
        message <- getLine
        
        if message == "exit"
        then do 
            putStrLn "Exited"
            return ()
        else do
            send socket $ pack message
            response <- recv socket 2048
            maybe noAck acked response
            where 
                noAck = putStrLn "No Message."
                acked r = do
                    putStrLn $ unpack r
                    mainLoop socket         
