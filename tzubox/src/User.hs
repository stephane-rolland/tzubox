module User where

import qualified Config as C
import qualified UserNetwork as UN
import qualified FileInfo as F


main :: IO ()
main = do
  userConfig <- C.readUserConfig

  fileInfos <- F.getFilesInfos userConfig
  
  putStrLn $ show fileInfos
  
  -- connects regularily to master
  UN.main
