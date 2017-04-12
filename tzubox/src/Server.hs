
module Server where

import Pipes 
import qualified Pipes.Binary as PipesBinary
import qualified Pipes.Network.TCP as PNT
import qualified Pipes.Parse as PP
import qualified Pipes.Prelude as PipesPrelude

import qualified MasterAsk

pageSize :: Int
pageSize = 4096


main :: IO ()
main = PNT.serve (PNT.Host "127.0.0.1") "23456" $
  \(connectionSocket, remoteAddress) -> do
                     putStrLn $ "Remote connection from ip = " ++ (show remoteAddress)
                     _ <- runEffect $ do
                       let bytesReceiver = PNT.fromSocket connectionSocket pageSize
                       let commandDecoder = PP.parsed PipesBinary.decode bytesReceiver
                       commandDecoder >-> PipesPrelude.mapM MasterAsk.sideffectHandler >-> for cat PipesBinary.encode >-> PNT.toSocket connectionSocket
                     return ()
