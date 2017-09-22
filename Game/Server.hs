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
    putStrLn hbar
    putStrLn $ "Gamemode is: " ++ (show . modeOf) state
    putStrLn $ "Clients: "  ++ (show . length . playersOf) state
    putStrLn $ "Id: " ++ (show . idNrOf) state
    putStrLn "Lobby Messages: "
    mapM putStrLn $ lobbyChatOf state
    mapM print $ playersOf state
    putStrLn hbar
    where 
        hbar = replicate 90 '-'



playerConnect gameState = do
    state <- takeMVar gameState
    let idNr = idNrOf state
    let player = newPlayer idNr $ "Duke" ++ show idNr
    let newPlayers = player : playersOf state 
    putMVar gameState $ state { playersOf = newPlayers
                              , idNrOf = idNr + 1 }
    return idNr



playerDisconnect gameState pId = do
    state <- takeMVar gameState
    let newPlayers = removeThisPlayerFrom $ playersOf state
    putMVar gameState $ state { playersOf = newPlayers }
    where 
        removeThisPlayerFrom = filter $ \p -> idOf p /= pId


-- TODO might want to merge the logic from Ready and Unready
-- TODO Also don't look at the readyness if the game has already started
handle gameState pId Ready = do
    putStrLn $ show pId ++ " Ready"
    state <- readMVar gameState
    let maybePlayer = find playerWithThisId (playersOf state)
    let ready = maybe False readinessOf maybePlayer
    if ready 
    then do
         display gameState
         return "You are already marked as ready."
    else do
         state <- takeMVar gameState
         let players = playersOf state
         let newPlayers = mapIf playerWithThisId setReady players
         let readyNr = length $ filter (\p -> readinessOf p == True) newPlayers
         let mode = if readyNr == length players
                    then Day
                    else Lobby
         putMVar gameState $ state { playersOf = newPlayers,
                                     modeOf = mode }
         display gameState
         return "You are marked as ready."
    where 
        playerWithThisId p = idOf p == pId
        setReady p = p {readinessOf = True}

handle gameState pId Unready = do
    putStrLn $ show pId ++ " Unready"
    state <- readMVar gameState
    let maybePlayer = find playerWithThisId (playersOf state)
    let ready = maybe True readinessOf maybePlayer
    if not ready 
    then do
         display gameState
         return "You are already marked as unready."
    else do
         state <- takeMVar gameState
         let players = playersOf state
         let newPlayers = mapIf playerWithThisId setUnready players
         putMVar gameState $ state {playersOf = newPlayers}
         display gameState
         return "You are marked as unready."
    where 
        playerWithThisId p = idOf p == pId
        setUnready p = p {readinessOf = False}

handle gameState pId (Say msg) = do
    state <- takeMVar gameState
    let newMessages = (msg ++ ": " ++ show pId) : lobbyChatOf state
    putMVar gameState $ state {lobbyChatOf = newMessages}
    display gameState
    return "Message Sent."

handle gameState _ GetMessages = do
    chat <- lobbyChatOf <$> readMVar gameState
    return $ unlines chat

handle gameState pId (Rename newName) = do
    state <- takeMVar gameState
    let maybePlayerWithName = find playerWithThisName $ playersOf state
    maybe (changePlayerName state) (playerExists state) maybePlayerWithName
    where 
        playerExists state _ = do
            putMVar gameState state
            return "That name is already taken."
    
        changePlayerName state = do
            let maybePlayer = find playerWithThisId $ playersOf state
            maybe (failure state) (rename state) maybePlayer
        
        failure state = do
            putMVar gameState state
            return "You do not exist."
            
        rename state player = do
            let players = filter (\p -> idOf p /= pId) $ playersOf state
            let newplayer = player {nameOf = newName}
            let newPlayers = newplayer : players
            putMVar gameState $ state {playersOf = newPlayers}
            display gameState
            return $ "Your name is now " ++ newName
            
        playerWithThisId p = idOf p == pId
        playerWithThisName p = nameOf p == newName

handle _ _ _ = do
    putStrLn "I forgott to pattern match!"
    return "I forgott to pattern match!"