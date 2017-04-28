module Master where

import qualified Config as C
import qualified Server
import qualified FileInfo as FI

import Control.Lens


main :: IO ()
main = do
  masterConfig <- C.readMasterConfig

  let d = view C.backupPath masterConfig 
  fileInfos <- FI.getFileInfos [d]

  putStrLn $ "content of master backup is = " ++ (show fileInfos)

  -- awaits connection from clients
  Server.main
  
