module User where

import qualified Config as C
import qualified Client


import Control.Lens

main :: IO ()
main = do

  ip <- getMasterIp
  Client.main ip -- getFileInfos -- connects regularily to master



getMasterIp :: IO String
getMasterIp = do
  userConfig <- C.readUserConfig

  let ip = view (C.masterip . C.ip) userConfig

  return ip


  
