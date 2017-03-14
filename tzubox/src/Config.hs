module Config where

import qualified System.Directory as SD
import qualified System.IO as SIO
import qualified UserConfig as U

pathConfigMaster :: String
pathConfigMaster = "/home/code/src/tzubox/simul/master/.master.cfg"

pathConfigUser :: String
pathConfigUser = "/home/code/src/tzubox/simul/user/.user.cfg"


isMaster :: IO (Bool)
isMaster = do
  isFileExists <- SD.doesFileExist pathConfigMaster
  return isFileExists
  
readUserConfig :: IO (U.UserConfig)
readUserConfig = do
  allLines <- SIO.readFile pathConfigUser
  let userConfig = U.parse $ lines $ allLines 
  return userConfig
