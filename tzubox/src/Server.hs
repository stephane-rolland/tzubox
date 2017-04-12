
module Server where

import Pipes 
import qualified Pipes.Binary as PipesBinary
import qualified Pipes.Network.TCP as PNT
import qualified Message as M
import qualified Pipes.Parse as PP
import qualified Pipes.Prelude as PipesPrelude

pageSize :: Int
pageSize = 4096

sideffectHandler :: MonadIO m => M.Message -> m M.Message
sideffectHandler c = do
  liftIO $ putStrLn $ "received message = " ++ (show c)
  return $ M.MasterMsg $ M.AskAllFileInfos

main :: IO ()
main = PNT.serve (PNT.Host "127.0.0.1") "23456" $
  \(connectionSocket, remoteAddress) -> do
                     putStrLn $ "Remote connection from ip = " ++ (show remoteAddress)
                     _ <- runEffect $ do
                       let bytesReceiver = PNT.fromSocket connectionSocket pageSize
                       let commandDecoder = PP.parsed PipesBinary.decode bytesReceiver
                       commandDecoder >-> PipesPrelude.mapM sideffectHandler >-> for cat PipesBinary.encode >-> PNT.toSocket connectionSocket
                     return ()
