module Common (
handleReceive,
sendTo,
logWith,
Logger,
LoggerMethod (..)
) where

import Network.Simple.TCP
import Data.ByteString.Char8 (pack, unpack)

handleReceive :: Socket -> IO () -> (String -> IO ()) -> IO ()
handleReceive socket disconnected msgReceived =
  recv socket 256 >>= maybe disconnected (msgReceived . unpack)

sendTo :: Socket -> String -> IO ()
sendTo socket msg = send socket $ pack msg

data LoggerMethod = Printer
type Logger = String -> IO ()

logWith :: LoggerMethod -> Logger
logWith Printer msg = putStrLn msg
