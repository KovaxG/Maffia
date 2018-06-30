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
        loop connectionSocket

loop :: Socket -> IO ()
loop connectionSocket = undefined 
