
module Client where

import Pipes
import qualified Pipes.Binary as PipesBinary
import qualified Pipes.Network.TCP as PNT
import qualified Pipes.Prelude as PipesPrelude
import qualified Pipes.Parse as PP
import qualified Message as M
--import qualified Network.Simple.TCP as NST
--import qualified Data.Binary as DB

pageSize :: Int
pageSize = 4096

sideffectHandler :: MonadIO m => M.Message -> m M.Message
sideffectHandler c = do
  liftIO $ putStrLn $ "received = " ++ (show c)
  return $ M.UserMsg $ M.EmptyMessage 

main :: String -> IO ()
main ip = PNT.connect ip "23456" $
  \(connectionSocket, remoteAddress) -> do
    putStrLn $ "Connected to distant server ip = " ++ (show remoteAddress)
    sendFirstMessage connectionSocket
    _ <- runEffect $ do
      let bytesReceiver = PNT.fromSocket connectionSocket pageSize
      let commandDecoder = PP.parsed PipesBinary.decode bytesReceiver
      commandDecoder >-> PipesPrelude.mapM sideffectHandler >-> for cat PipesBinary.encode >-> PNT.toSocket connectionSocket 
    return ()

sendFirstMessage :: PNT.Socket -> IO ()
sendFirstMessage s = do
  _ <- runEffect $ do
    let encodedProducer = PipesBinary.encode $ M.UserMsg $ M.FirstMessage "code" 
    encodedProducer >-> PNT.toSocket s  
  return ()
