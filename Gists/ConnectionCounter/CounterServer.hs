-- Simple Server Example

import Network.Simple.TCP
import Data.ByteString.Char8 (pack, unpack)
import Control.Concurrent.MVar

main = do
    counter <- newMVar 0
    putStrLn "Listening for connections."
    serve (Host "localhost") "8080" $ \(connectionSocket, remoteAddr) -> do
        putStrLn $ ((bracket . show) remoteAddr) ++ "Connected."
        incCounter counter
        printClientNr counter
        listenTo connectionSocket counter $ show remoteAddr
        
listenTo socket counter id = do
    msg <- recv socket 256
    maybe nothingReceived dataReceived msg
    where 
        nothingReceived = do
            decCounter counter
            putStrLn $ bracket id ++ "Disconnected."
            printClientNr counter
    
        dataReceived byteString = do
            let received = unpack byteString
            putStrLn $ bracket id ++ received
            send socket $ pack "ack"
            listenTo socket counter id
            
printClientNr counter = do
    nr <- takeMVar counter
    putMVar counter nr
    putStrLn $ "Clients: " ++ show nr
    
incCounter counter = do
    nr <- takeMVar counter
    putMVar counter $ nr + 1
    
decCounter counter = do
    nr <- takeMVar counter
    putMVar counter $ nr - 1
    
bracket s = "[" ++ s ++ "] "