module Server where

import Network.Simple.TCP
import Data.ByteString.Char8 (pack, unpack)
import Control.Concurrent.MVar
import Utils
import Text.Read (readMaybe)
import Data.List (find)

main = do
    gameState <- newMVar initialState
    putStrLn "Listening for connections."
    serve (Host "localhost") "8080" $ \(connectionSocket, remoteAddr) -> do
        pId <- playerConnect gameState
        display gameState
        listenTo connectionSocket gameState pId


listenTo socket gameState pId = do
    msg <- recv socket 256
    maybe nothingReceived dataReceived msg
    where
        nothingReceived = do
            playerDisconnect gameState pId
            putStrLn $ show pId ++ " Disconnected."
            display gameState
            return ()

        dataReceived byteString = do
            let received = readMaybe $ unpack byteString :: Maybe Message
            response <- maybe (return "Not a command") (handle gameState pId) received
            send socket $ pack response
            listenTo socket gameState pId


display gameState = do
    state <- readMVar gameState
    putStrLn $ "Clients: "  ++ (show . length . playersOf) state
    putStrLn $ "Id: " ++ (show . idNrOf) state
    putStrLn "Lobby Messages: "
    mapM_ putStrLn $ lobbyChatOf state
    mapM_ (putStrLn . show) $ playersOf state
    putStrLn ""


playerConnect gameState = do
    state <- takeMVar gameState
    let newIdNr = idNrOf state + 1
    let player = newPlayer (idNrOf state) ("Duke" ++ show (idNrOf state) ++ " ")
    let newPlayers = player : playersOf state 
    putMVar gameState $ state { playersOf = newPlayers
                              , idNrOf = newIdNr }
    return $ idNrOf state


playerDisconnect gameState pId = do
    state <- takeMVar gameState
    let newPlayers = filter (\p -> idOf p /= pId) $ playersOf state
    putMVar gameState $ state { playersOf = newPlayers }


handle gameState pId Ready = do
    -- TODO might want to check first if player is already ready
    putStrLn $ show pId ++ " Ready"
    state <- takeMVar gameState
    let players = playersOf state
    let newPlayers = map (\p -> if idOf p == pId then p {readyOf = True} else p) players
    putMVar gameState $ state {playersOf = newPlayers}
    display gameState
    return "Ack"
handle gameState pId Unready = do
    -- TODO might want to check first if player is already unready
    putStrLn $ show pId ++ " Unready"
    state <- takeMVar gameState
    let players = playersOf state
    let newPlayers = map (\p -> if idOf p == pId then p {readyOf = False} else p) players
    putMVar gameState $ state {playersOf = newPlayers}
    display gameState
    return "Ack"
handle gameState pId (Say msg) = do
    state <- takeMVar gameState
    let newMessages = (show pId ++ ": " ++ msg) : lobbyChatOf state
    putMVar gameState $ state {lobbyChatOf = newMessages}
    display gameState
    return "Ack"
handle gameState _ (GetMessages _) = do
    chat <- lobbyChatOf <$> readMVar gameState
    return $ unlines chat
handle gameState pId (Rename newName) = do
    --TODO check so that there are no duplicate names
    state <- takeMVar gameState
    let maybePlayer = find (\p -> idOf p == pId) $ playersOf state
    maybe (failure state) (rename state) maybePlayer
    where 
        failure state = do
            putMVar gameState state
            return "Failure"
        rename state player = do
            let players = filter (\p -> idOf p /= pId) $ playersOf state
            let newplayer = player { nameOf = newName }
            let newPlayers = newplayer : players
            putMVar gameState $ state {playersOf = newPlayers}
            display gameState
            return "Ack"
handle _ _ _ = do
    putStrLn "I forgott to pattern match!"
    return "I forgott to pattern match!"