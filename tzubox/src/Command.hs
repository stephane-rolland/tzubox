
{-# LANGUAGE DeriveGeneric #-}
module Command where
import Data.Binary
import GHC.Generics (Generic)

data Command = FirstMessage
               | DoNothing
               | DoSomething Int
               deriving (Show,Generic)

instance Binary Command

