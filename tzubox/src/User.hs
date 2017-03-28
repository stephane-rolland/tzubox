module User where

import qualified Config as C
import qualified UserNetwork as UN
import qualified FileInfo as F

import Control.Lens

main :: IO ()
main = do
  userConfig <- C.readUserConfig

  let dirs = view C.paths userConfig
  fileInfos <- F.getFilesInfos dirs
  
  putStrLn $ show fileInfos
  
  -- connects regularily to master
  UN.main
