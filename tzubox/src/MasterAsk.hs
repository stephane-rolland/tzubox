module MasterAsk where

import qualified Control.Monad.IO.Class as CMIO

import qualified Message as M


sideffectHandler :: CMIO.MonadIO m => M.Message -> m M.Message
sideffectHandler c = do
  CMIO.liftIO $ putStrLn $ "received message = " ++ (show c)
  return $ M.MasterMsg $ M.AskAllFileInfos
