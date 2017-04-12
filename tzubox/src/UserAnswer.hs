module UserAnswer where

import qualified Data.Time.Clock as DTC

import Message

import qualified Control.Monad.IO.Class as CMIO
import Control.Type.Operator
  
sideffectHandler :: CMIO.MonadIO m => Message -> m Message
sideffectHandler (MasterMsg m) = do
  CMIO.liftIO $ putStrLn $ "received = " ++ (show m)
  msg <- CMIO.liftIO $ answer m
  return $ UserMsg $ msg
sideffectHandler _ = error "this is not a message meant to be received from master"

answer :: MasterMessage -> IO $ UserMessage
answer AskTimeNow = askTimeNow >>= \t -> (return $ AnswerTimeNow t) 
answer _ = error "this message is not yet understood by user, implement it please"


askTimeNow :: IO(DTC.UTCTime)
askTimeNow = do
  currentTime <- DTC.getCurrentTime
  return $ currentTime
