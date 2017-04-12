module User where

import qualified Config as C
import qualified Client
import qualified FileInfo as F

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


getFileInfos :: IO F.FileInfos
getFileInfos = do
  userConfig <- C.readUserConfig

  let dirs = view C.paths userConfig
  fileInfos <- F.getFilesInfos dirs
  
  putStrLn $ show fileInfos

  return fileInfos
  
