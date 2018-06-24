module Server where

import Network.Simple.TCP
import Data.ByteString.Char8 (pack, unpack)
import Control.Concurrent.MVar
import Text.Read (readMaybe)
import Data.List (find)
import Debug.Trace

import Utils
import MonitorLogic

type PlayerId = Int

main :: IO ()
main = do
    gameState <- newMVar initialState
    monitorVars <- newMVar initialMonitorState
    
    putStrLn "Listening for connections."
    serve (Host "localhost") "8080" $ \(connectionSocket, remoteAddr) -> do
        mainLoop connectionSocket
        -- Remove this block, and replace it with the mainLoop
        _msg <- recv connectionSocket 256
        let msg = maybe "bla" unpack  _msg
        if msg == "monitor"
        then do
             send connectionSocket $ pack "Connected as monitor"
             mId <- monitorConnect monitorVars
             monitorStuff connectionSocket mId monitorVars
        else do
             send connectionSocket $ pack "Connected as player"
             pId <- playerConnect gameState
             display gameState
             listenTo connectionSocket gameState pId monitorVars


-- There should be only one recv and readMVar at the start
-- and a send and setMVar at the end. In between the logic
-- should be totally pure.
mainLoop :: Socket -> IO ()
mainLoop socket = do
    received <- incommingMessage socket
    if received == "monitor"
    then monitorLogic
    else playerLogic
    mainLoop socket
    
    
playerLogic :: IO ()
playerLogic = putStrLn "Doing Player Logic" 
    
    
incommingMessage :: Socket -> IO String
incommingMessage socket = decodeMsg <$> recv socket 512
    where decodeMsg = maybe decodeFail decodeSucc
          decodeFail = "Could not decode message."
          decodeSucc = unpack
    
             
monitorStuff :: Socket -> PlayerId -> MVar MonitorState -> IO ()
monitorStuff socket mId monitorVars = do
    putStrLn $ "Monitor Connected: " ++ show mId
    monitorLoop socket mId monitorVars


monitorLoop :: Socket -> PlayerId -> MVar MonitorState -> IO ()    
monitorLoop socket mId monitorVars = do 
    mvars <- takeMVar monitorVars
    let thisMVar = find (\t -> fst t == mId) (varsOf mvars)
    mvar <- maybe notFound found thisMVar
    let iAmTired = filter (\t -> fst t /= mId) (varsOf mvars)
    let iCantThinkOfAName = iAmTired ++ [(mId, mvar)]
    putMVar monitorVars $ mvars { varsOf = iCantThinkOfAName }
    monitorLoop socket mId monitorVars
    where
        notFound = undefined
        found (_, mvar) = do
            msg <- takeMVar mvar
            send socket $ pack msg
            return mvar
    

monitorConnect :: MVar MonitorState -> IO PlayerId 
monitorConnect monitorVars = do
    mvars <- takeMVar monitorVars
    let id = midNrOf mvars
    newMonitorMVar <- newMVar "Joined Chat"
    let newVarsOf = varsOf mvars ++ [(id, newMonitorMVar)]
    putMVar monitorVars $ mvars { midNrOf = id + 1,
                                  varsOf =  newVarsOf }
    return id


listenTo :: Socket -> MVar State -> PlayerId -> MVar MonitorState -> IO ()    
listenTo socket gameState pId monitorVars = do
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
            response <- maybe (return "Not a command") (handle gameState pId monitorVars) received
            send socket $ pack response
            listenTo socket gameState pId monitorVars



display :: MVar State -> IO ()            
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



playerConnect :: MVar State -> IO PlayerId       
playerConnect gameState = do
    state <- takeMVar gameState
    let idNr = idNrOf state
    let player = newPlayer idNr $ "Duke" ++ show idNr
    let newPlayers = player : playersOf state 
    putMVar gameState $ state { playersOf = newPlayers
                              , idNrOf = idNr + 1 }
    return idNr


    
playerDisconnect :: MVar State -> PlayerId -> IO ()    
playerDisconnect gameState pId = do
    state <- takeMVar gameState
    let newPlayers = removeThisPlayerFrom $ playersOf state
    putMVar gameState $ state { playersOf = newPlayers }
    where 
        removeThisPlayerFrom = filter $ \p -> idOf p /= pId



handle :: MVar State -> PlayerId -> MVar MonitorState -> Message -> IO String      
-- TODO might want to merge the logic from Ready and Unready
-- TODO Also don't look at the readyness if the game has already started
handle gameState pId _ Ready = do
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

handle gameState pId _ Unready = do
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

handle gameState pId monitorVars (Say msg) = do
    state <- takeMVar gameState
    let newMessages = (msg ++ ": " ++ show pId) : lobbyChatOf state
    putMVar gameState $ state {lobbyChatOf = newMessages}
    
    mstate <- takeMVar monitorVars
    
    _ <- sequence $ fmap (flip putMVar msg) (map (\(a, b) -> b) (varsOf mstate))
    
    -- I need to update all the vars in this list, put the message in each one
    -- how the hell do I do that? I need an IO monad, but then I need to get
    -- rid of it. How do I do it? I guess I could do a function that maps an IO
    -- monad onto each element of the list, then extract it somehow with a 
    -- traverse or something. IDK, too tired. Good luck future Gyuri.
    -- let blabla = (\(i, v) -> (i, )) `map` (varsOf mstate)
    putMVar monitorVars $ mstate
    
    display gameState
    return "Message Sent."
    where   
        swap f a b = f b a

handle gameState _ _ GetMessages = do
    chat <- lobbyChatOf <$> readMVar gameState
    return $ unlines chat

handle gameState pId _ (Rename newName) = do
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

handle _ _ _ _ = do
    putStrLn "I forgott to pattern match!"
    return "I forgott to pattern match!"