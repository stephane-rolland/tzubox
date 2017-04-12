
{-# LANGUAGE DeriveGeneric #-}
module Message where
import Data.Binary
import GHC.Generics (Generic)

import qualified FileInfo as FI

data Message = UserMsg UserMessage
               | MasterMsg MasterMessage
               deriving(Show,Generic)

type UserName = String

data UserMessage =
  EmptyMessage
  | FirstMessage UserName  
  | AnswerFileInfos UserName FI.FileInfos 
  deriving(Show,Generic)

data MasterMessage = 
  AskAllFileInfos
  deriving(Show,Generic)


instance Binary Message
instance Binary UserMessage
instance Binary MasterMessage
