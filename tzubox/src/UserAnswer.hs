module UserAnswer where

import qualified Data.Time.Clock as DTC

import Message
import Config

import qualified Control.Monad.IO.Class as CMIO
import Control.Type.Operator
import Control.Lens
import qualified FileInfo as F

sideffectHandler :: CMIO.MonadIO m => Message -> m Message
sideffectHandler (MasterMsg m) = do
  CMIO.liftIO $ putStrLn $ "received = " ++ (show m)
  msg <- CMIO.liftIO $ answer m
  return $ UserMsg $ msg
sideffectHandler _ = error "this is not a message meant to be received from master"

answer :: MasterMessage -> IO $ UserMessage
answer AskTimeNow = do
  t <- askTimeNow
  cfg <- readUserConfig
  let uname = view username cfg
  let msg = AnswerTimeNow uname t
  return msg
answer AskAllFileInfos = do
  t <- askTimeNow
  cfg <- readUserConfig
  let uname = view username cfg
  fileInfos <- getFileInfos
  let msg = AnswerFileInfos uname t fileInfos
  return msg
  
answer _ = error "this message is not yet understood by user, implement it please"


askTimeNow :: IO DTC.UTCTime
askTimeNow = do
  currentTime <- DTC.getCurrentTime
  return $ currentTime

getFileInfos :: IO F.FileInfos
getFileInfos = do
  userConfig <- readUserConfig

  let dirs = view paths userConfig
  fileInfos <- F.getFileInfos dirs
  
  putStrLn $ show fileInfos

  return fileInfos
