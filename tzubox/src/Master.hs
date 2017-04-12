module Master where

import qualified Config as C
import qualified Server
import qualified FileInfo as F

import Control.Lens


main :: IO ()
main = do
  masterConfig <- C.readMasterConfig

  let d = view C.backupPath masterConfig 
  fileInfos <- F.getFilesInfos [d]

  putStrLn $ show fileInfos

  -- awaits connection from clients
  Server.main
  
