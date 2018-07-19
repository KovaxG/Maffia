module Common (
handleReceive,
sendTo,
logWith,
Logger,
LoggerMethod (..),
sleepForever
) where

import Network.Simple.TCP
import Data.ByteString.Char8 (pack, unpack)
import Control.Concurrent.Thread.Delay

-- TODO use the Player datatype instead of socket!
handleReceive :: Socket -> IO a -> (String -> IO a) -> IO a
handleReceive socket disconnected msgReceived =
  recv socket 256 >>= maybe disconnected (msgReceived . unpack)

sendTo :: Socket -> String -> IO ()
sendTo socket msg = send socket $ pack msg

data LoggerMethod = Printer
type Logger = String -> IO ()

logWith :: LoggerMethod -> Logger
logWith Printer msg = putStrLn msg

sleepForever :: IO ()
sleepForever = do
  delay 100000000000000000
  sleepForever
