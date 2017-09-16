-- Simple Server Example

import Network.Simple.TCP
import Data.ByteString.Char8 (pack, unpack)

main = do
    print "Listening for connections."
    serve (Host "localhost") "8080" $ \(connectionSocket, remoteAddr) -> do
        print $ "TCP connection established from " ++ show remoteAddr
        listenTo connectionSocket
        
listenTo socket = do
    msg <- recv socket 256
    maybe nothingReceived dataReceived msg
    where 
        nothingReceived = print "Disconnected."
    
        dataReceived byteString = do
            let string = unpack byteString
            print $ "Received: " ++ string
            
            let message = "Hello Client, I am Server."
            send socket $ pack message
            print $ "Sent: " ++ message
            
            listenTo socket
