
{-# LANGUAGE DeriveGeneric #-}
module Message where
import Data.Binary
import Data.Binary.Orphans()
import GHC.Generics (Generic)

import qualified FileInfo as FI
import qualified Data.Time.Clock as DTC


data Message = UserMsg UserMessage
               | MasterMsg MasterMessage
               deriving(Show,Generic)

type UserName = String

data UserMessage =
  EmptyMessage
  | FirstMessage UserName  
  | AnswerTimeNow DTC.UTCTime
  | AnswerFileInfos UserName FI.FileInfos 
  deriving(Show,Generic)

data MasterMessage = 
  AskTimeNow 
  | AskAllFileInfos
  deriving(Show,Generic)


instance Binary Message
instance Binary UserMessage
instance Binary MasterMessage
