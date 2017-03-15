module UserNetwork where

import Network.Socket hiding (recv)

import qualified Network as N
import qualified System.IO as SIO

import Command
import qualified Control.Monad as CM
import qualified Control.Concurrent.Timer as CCT
import qualified Control.Concurrent.Suspend.Lifted as CCSL
import qualified Control.Concurrent as CC
import qualified Data.Maybe as DM

connectionPort :: N.PortID
connectionPort = N.PortNumber 33336

connectNetwork :: String -> IO(Maybe SIO.Handle)
connectNetwork target = withSocketsDo $
  do
    handle <- N.connectTo target connectionPort
    SIO.hSetBuffering handle SIO.LineBuffering
    return $ Just handle

sendOverNetwork :: SIO.Handle -> Command -> IO ()
sendOverNetwork handle cmd = do
    SIO.hPutStrLn handle $ show cmd

receiveOverNetwork :: (String -> IO()) -> SIO.Handle -> IO ()
receiveOverNetwork treatMessage handle = do
  _ <- CM.forever $ do
    msg <- getNextMessage handle
    treatMessage msg
  return ()

getNextMessage :: SIO.Handle -> IO(String)
getNextMessage handle = do
    msg <- SIO.hGetLine handle
    return msg

  
main :: IO ()
main = do
  let delay = CCSL.mDelay 1
  _ <- CCT.repeatedTimer (onTimerClockNotification) delay
  CM.forever $ do
    CC.threadDelay 30000 
    

onTimerClockNotification :: IO ()
onTimerClockNotification = do
  putStrLn "on timer"
  maybeHandle <- connectNetwork "localhost"
  let h = DM.fromJust maybeHandle
  sendOverNetwork h EmptyMessage
