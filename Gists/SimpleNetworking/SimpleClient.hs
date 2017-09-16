-- Simple Client Example

import Network.Simple.TCP
import Data.ByteString.Char8 (pack, unpack)

main = do 
    print "Connecting to server."
    connect "localhost" "8080" $ \(serverSocket, sockaddr) -> do
        print $ "Succesfully connected to " ++ show sockaddr
        let message = "hello server, I am client"
        send serverSocket $ pack message
        print $ "Sent: " ++ message
        
        received <- recv serverSocket 256
        let response = maybe "No Message." unpack received
        print $ "Received: " ++ response
        print "Disconnected"
        
        