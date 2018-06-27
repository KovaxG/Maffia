-- Simple Server Example

import Network.Simple.TCP
import Data.ByteString.Char8 (pack, unpack)
import Control.Concurrent.MVar

main = do
    broadcast <- newMVar []
    putStrLn "Listening for connections."
    serve (Host "localhost") "8080" $ \(connectionSocket, remoteAddr) -> do
        putStrLn $ ((bracket . show) remoteAddr) ++ "Connected."
        addAddress broadcast connectionSocket
        putStrLn $ "Socket added for " ++ bracket (show remoteAddr)
        listenTo connectionSocket broadcast $ show remoteAddr

listenTo socket broadcast id = do
    msg <- recv socket 256
    maybe nothingReceived (dataReceived broadcast) msg
    where
        nothingReceived = putStrLn "Disconnected"

        dataReceived broadcast byteString = do
            let received = unpack byteString
            putStrLn $ bracket id ++ received
            send socket $ pack "ack"
            sockets <- takeMVar broadcast
            mapM_ (\s -> send s (pack received)) sockets
            putMVar broadcast sockets
            listenTo socket broadcast id

addAddress broadcast socket = do
    sockets <- takeMVar broadcast
    putMVar broadcast $ socket : sockets

bracket s = "[" ++ s ++ "] "
