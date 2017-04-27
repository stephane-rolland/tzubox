{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module FileInfo where
import qualified Config as C
--import qualified System.Posix.Time as SPT
--import qualified System.Posix.Types as SPTy
--import qualified System.Posix.Files as SPF
--import qualified System.Directory as SD
--import qualified Filesystem.Path as FP
-- import qualified System.Path as SP
import qualified System.Command as SC
import qualified System.IO as SIO

import qualified Control.Monad as CM
import Control.Lens
import Data.List as DL

import Data.Binary
import Control.DeepSeq
import GHC.Generics (Generic)

data FileInfo = FileInfo
  {
      _configPath :: String 
    , _filePath :: String
    , _modifyTime :: String
    , _changeTime :: String
  } deriving (Generic)

makeLenses ''FileInfo

instance Binary FileInfo
instance NFData FileInfo

type FileInfos = [FileInfo]

instance Show FileInfo where
  show (FileInfo a b c d) = "FileInfo " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (show d) 


getFileInfos :: C.Paths -> IO FileInfos
getFileInfos dirs = do
  fileInfos <- CM.mapM getFileInfosForDirectory dirs
  return $ concat $ fileInfos

getFileInfosForDirectory :: C.Path -> IO FileInfos
getFileInfosForDirectory pth = do
  let cmd = "ls"
  let args = ["-lRa"]
  (_, Just hout, _, _) <- SC.createProcess (SC.proc cmd args) { SC.cwd = Just dirpath,
                                                                SC.std_out = SC.CreatePipe }
  content <- SIO.hGetContents hout

  fileInfos <- parseLsContentAndGetStats content dirpath
  
  return fileInfos
  where
    dirpath = view C.directory pth

getStatForFile :: String -> String -> IO FileInfo
getStatForFile configpath filepath = do
  (_, Just hout, _, _) <- SC.createProcess (SC.proc cmd args) { SC.cwd = Just configpath,
                                                                SC.std_out = SC.CreatePipe }
  cnt <- SIO.hGetContents hout
  let (modTime, chgTime) = parseStatContent cnt

  return $ FileInfo configpath filepath modTime chgTime
  where
    cmd = "stat"
    args = [filepath]


parseStatContent :: String -> (String,String)
parseStatContent content = (modTime, chgTime)
  where
    lst = lines content
    modTimeLine = lst !! 5
    chgTimeLine =  lst !! 6
    modTime = drop 8 modTimeLine
    chgTime = drop 8 chgTimeLine

  
parseLsContentAndGetStats :: String -> String -> IO FileInfos
parseLsContentAndGetStats lsContent dirpath = do
  filesInfos <- CM.mapM (getStatForFile dirpath) filepaths 
  return filesInfos
  where
    filepaths = parseLsContent lsContent
 
parseLsContent :: String -> [String]
parseLsContent cnt = filter predicate fs
  where
    ls = lines cnt
    fs = map parseLsLine ls
    predicate ""   = False
    predicate "."  = False
    predicate ".." = False
    predicate _ = True

parseLsLine :: String -> String
parseLsLine l = if length ws <= 2 then ""
                else filepath
  where
    ws = words l
    filepath = DL.intercalate " " $ drop 8 ws
  
