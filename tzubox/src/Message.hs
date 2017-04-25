
{-# LANGUAGE DeriveGeneric #-}
module Message where
import Data.Binary
import Data.Binary.Orphans()
import GHC.Generics (Generic)

import qualified FileInfo as FI
import qualified FileBinary as FB
import qualified Data.Time.Clock as DTC


data Message = UserMsg UserMessage
               | MasterMsg MasterMessage
               deriving(Show,Generic)

type UserName = String

data UserMessage =
  FirstMessage UserName  
  | AnswerTimeNow UserName DTC.UTCTime
  | AnswerFileInfos UserName DTC.UTCTime FI.FileInfos
  | AnswerUserFilesToUpdate UserName FB.FileBinaries
  deriving(Show,Generic)

data MasterMessage = 
  AskTimeNow 
  | AskAllFileInfos
  | AskUserUpdateFile FI.FileInfos
  | AskMasterUpdateFile FB.FileBinaries
  | AskUserDeleteFile FI.FileInfos
  | AskWaitSomeTimeBeforeNextSynchro
  deriving(Show,Generic)


instance Binary Message
instance Binary UserMessage
instance Binary MasterMessage
