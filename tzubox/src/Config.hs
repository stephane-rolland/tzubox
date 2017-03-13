module Config where

import qualified System.Directory as SD

pathConfigMaster :: String
pathConfigMaster = "/home/code/src/tzubox/simul/master/user.cfg"


isMaster :: IO (Bool)
isMaster = do
  isFileExists <- SD.doesFileExist pathConfigMaster
  return isFileExists
  
