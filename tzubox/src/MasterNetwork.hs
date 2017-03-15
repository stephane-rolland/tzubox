module MasterNetwork where

import Network
import System.IO

import Command
import qualified Control.Monad as CM

main :: IO ()
main = withSocketsDo $ do
    socketCommands <- listenOn $ portNumberCommands
    listenCommands socketCommands  

listenCommands :: Socket -> IO ()
listenCommands socketCommands = CM.forever $ do
   (handleCommands, _, _) <- accept socketCommands
   respondToCommands handleCommands

-- notifier :: Handle -> SubscriberNotification
-- notifier h s = do
--      hPutStrLn h s
--     hFlush h

respondToCommands :: Handle -> IO()     
respondToCommands h = do 
       input <- hGetLine h
       let inputCommand = read input :: Command
       putStrLn $ "Msg has been received = " ++ (show inputCommand)

       hPutStrLn h $ show EmptyMessage
       hFlush h

       putStrLn $ "Response sent back"


portNumberCommands :: PortID
portNumberCommands = PortNumber 33336







