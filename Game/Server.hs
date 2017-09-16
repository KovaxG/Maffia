module Server where

import Network.Simple.TCP
import Data.ByteString.Char8 (pack, unpack)
import Control.Concurrent.MVar
import Utils

main = do
    gameState <- newMVar initialState
    putStrLn "Listening for connections."
    serve (Host "localhost") "8080" $ \(connectionSocket, remoteAddr) -> do
        player <- playerConnect gameState
        display gameState
        listenTo connectionSocket gameState player

listenTo socket gameState player = do
    msg <- recv socket 256
    maybe nothingReceived dataReceived msg
    where
        nothingReceived = do
            playerDisconnect gameState player
            putStrLn $ nameOf player ++ "Disconnected."
            display gameState

        dataReceived byteString = do
            let received = unpack byteString
            putStrLn $ nameOf player ++ received
            send socket $ pack "ack"
            listenTo socket gameState player

display gameState = do
    state <- takeMVar gameState
    putMVar gameState state
    putStrLn $ "Clients: "  ++ show (playerNrOf state)
    putStrLn $ "Id: " ++ (show . idNrOf) state
    mapM_ (putStrLn . show) $ playersOf state
    putStrLn ""

playerConnect gameState = do
    state <- takeMVar gameState
    let newPlayerNr = playerNrOf state + 1
    let newIdNr = idNrOf state + 1
    let player = newPlayer (idNrOf state) ("Duke" ++ show (idNrOf state) ++ " ")
    let newPlayers = player : playersOf state 
    putMVar gameState $ state { playerNrOf = newPlayerNr
                              , idNrOf = newIdNr 
                              , playersOf = newPlayers}
    return player

playerDisconnect gameState player = do
    state <- takeMVar gameState
    let newPlayerNr = playerNrOf state - 1
    let newPlayers = filter (/= player) $ playersOf state
    putMVar gameState $ state { playerNrOf = newPlayerNr 
                              , playersOf = newPlayers }
