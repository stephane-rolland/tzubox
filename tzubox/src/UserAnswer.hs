module UserAnswer where

import qualified Data.Time.Clock as DTC

import Message
import Config

import qualified Control.Monad.IO.Class as CMIO
import Control.Type.Operator
import Control.Lens
import qualified FileInfo as F

import qualified Control.Monad as CM
import qualified FileBinary as FB

import qualified Control.DeepSeq as CDS

sideffectHandler :: CMIO.MonadIO m => Message -> m Message
sideffectHandler (MasterMsg m) = do
  CMIO.liftIO $ putStrLn $ "received = " ++ (show m)
  msg <- CMIO.liftIO $ answerFor m
  return $ UserMsg $ msg
sideffectHandler _ = error "this is not a message meant to be received from master"

answerFor :: MasterMessage -> IO $ UserMessage
answerFor AskTimeNow = do
  t <- askTimeNow
  cfg <- readUserConfig
  let uname = view username cfg
  let msg = AnswerTimeNow uname t
  return msg
answerFor AskAllFileInfos = do
  t <- askTimeNow
  cfg <- readUserConfig
  let uname = view username cfg
  fileInfos <- getFileInfos
  let msg = AnswerFileInfos uname t fileInfos
  return msg
answerFor (AskUserUpdateFile ls) = do
  cfg <- readUserConfig
  let uname = view username cfg
  putStrLn $ "nb files to retrieve = " ++ (show $ length ls)
  fileBinaries <- CM.mapM FB.getFileBinary ls
  let !toSend = fileBinaries `CDS.deepseq` fileBinaries
  putStrLn $ "ready to send " ++ (show $ length fileBinaries) ++ " binaries = " ++ (show fileBinaries) 
  return $ AnswerUserFilesToUpdate uname toSend
answerFor _ = error "this message is not yet understood by user, implement it please"

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
