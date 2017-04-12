
module Client where

import Pipes
import qualified Pipes.Binary as PipesBinary
import qualified Pipes.Network.TCP as PNT
import qualified Pipes.Prelude as PipesPrelude
import qualified Pipes.Parse as PP
import qualified Message as M
import qualified UserAnswer

pageSize :: Int
pageSize = 4096


main :: String -> IO ()
main ip = PNT.connect ip "23456" $
  \(connectionSocket, remoteAddress) -> do
    putStrLn $ "Connected to distant server ip = " ++ (show remoteAddress)
    sendFirstMessage connectionSocket
    _ <- runEffect $ do
      let bytesReceiver = PNT.fromSocket connectionSocket pageSize
      let commandDecoder = PP.parsed PipesBinary.decode bytesReceiver
      commandDecoder >-> PipesPrelude.mapM UserAnswer.sideffectHandler >-> for cat PipesBinary.encode >-> PNT.toSocket connectionSocket 
    return ()

sendFirstMessage :: PNT.Socket -> IO ()
sendFirstMessage s = do
  _ <- runEffect $ do
    let encodedProducer = PipesBinary.encode $ M.UserMsg $ M.FirstMessage "code" 
    encodedProducer >-> PNT.toSocket s  
  return ()
