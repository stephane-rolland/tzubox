module MasterAsk where

import qualified Control.Monad.IO.Class as CMIO

import Message 
import qualified Data.Time.Clock as DTC
import qualified Data.Time.Calendar as DTCA

import Control.Type.Operator
import Control.Lens
import qualified Control.Monad as CM

import qualified Config as C

import qualified System.Directory as SD
import qualified System.IO as SIO

import qualified FileInfo as FI
import qualified FileBinary as FB

import qualified Data.List as DL
import qualified Data.Maybe as DM
import qualified Data.ByteString as DBS

import System.FilePath.Posix as SFPP

type Time = DTC.UTCTime
type File = FI.FileInfo
type Files = [File]
type Day = DTCA.Day

sideffectHandler :: CMIO.MonadIO m => Message -> m Message
sideffectHandler (UserMsg um) = do
  CMIO.liftIO $ putStrLn $ "received user message = " ++ (show um)
  msg <- CMIO.liftIO $ askAfter um
  return $ MasterMsg $ msg
sideffectHandler _ = error "only user message can be processed, that's not what has been received"

askAfter :: UserMessage -> IO $ MasterMessage
askAfter (FirstMessage uname) = do
  CMIO.liftIO $ putStrLn $ "user connected = " ++ uname
  return AskTimeNow
askAfter (AnswerTimeNow uname t) = do
  serverTime <- DTC.getCurrentTime
  return AskAllFileInfos
askAfter (AnswerFileInfos uname t fileInfos) = do
  lastSynchroTime <- getLastSynchroTime uname
  checkUserBackupDirectory uname
  putStrLn $ "last synchro was at = " ++ (show lastSynchroTime)
  (ufsUpdate, ufsDelete, mfsUpdate, mfsDelete) <- getFileUpdatesToDo uname t fileInfos lastSynchroTime
  -- don't delete files for the moment
  let listUfUpdate = ufsUpdate
  let listMfUpdate = mfsUpdate
  let msg = getNextUpdateMessage listUfUpdate listMfUpdate
  _ <- case msg of
         AskWaitSomeTimeBeforeNextSynchro -> do
           setLastSynchroTime uname
         _ -> return ()
  return $ msg
askAfter (AnswerUserFilesToUpdate uname fbs) = do
  updateUserFilesInBackup uname fbs
  setLastSynchroTime uname
  return AskWaitSomeTimeBeforeNextSynchro
  
askAfter m = error $ "this message is not yet understood by master, implement it please " ++ (show m)

-- check the user backup directory exists, and creates it if not
checkUserBackupDirectory :: UserName -> IO ()
checkUserBackupDirectory uname = do
  dirpath <- getUserFilesPath uname
  SD.createDirectoryIfMissing True dirpath
  

-- TODO, this should be searching for the most recent file, not the .synchro file
getLastSynchroTime :: UserName -> IO $ Maybe $ Time
getLastSynchroTime uname = do
  dirpath <- getUserFilesPath uname
  let filename = dirpath ++ ".synchro"
  isExisting <- SD.doesFileExist filename
  ret <- case isExisting of
        False -> return Nothing
        True -> Just <$> readSynchroFile filename
  return ret
  
readSynchroFile :: String -> IO Time
readSynchroFile pth = do
  cnt <- SIO.readFile pth
  let allLines = lines cnt
  let firstLine = head allLines
  let synchroTime = read firstLine :: Time
  return synchroTime

setLastSynchroTime :: UserName -> IO ()
setLastSynchroTime uname = do
  dirpath <- getUserFilesPath uname
  let filename = dirpath ++ ".synchro"
  synchroTime <- DTC.getCurrentTime
  SIO.writeFile filename $ show synchroTime 

getNextUpdateMessage :: Files -> Files -> MasterMessage
getNextUpdateMessage [] [] = AskWaitSomeTimeBeforeNextSynchro
--getNextUpdateMessage [] ls = AskMasterUpdateFile ls
getNextUpdateMessage ls _ = AskUserUpdateFile ls
-- TODO HERE it would be GOOD to choose the oldest files to update first    

getUserFilesPath :: String -> IO String
getUserFilesPath uname = do
  cfg <- C.readMasterConfig
  let path = view C.backupPath cfg
  let dirpath = (view C.directory path) ++ "/" ++ uname
  return dirpath

type FileUpdates = (Files,Files,Files,Files)

getFileUpdatesToDo :: String -> Time -> Files -> Maybe Time -> IO FileUpdates 
getFileUpdatesToDo uname uTime uFileInfos lastSynchroTime = do
  userFilesPath <- getUserFilesPath uname
  let pth = C.Path $ userFilesPath 
  putStrLn $ "retrieving current files on master in path = " ++ (show pth)
  masterFileInfos <- FI.getFileInfos [pth]
  putStrLn $ "current files on master are = " ++ (show masterFileInfos)

  -- depending on modify/change time => select which files are to be updated or deleted
  let ufsUpdate = getUserFilesUpdate uTime uFileInfos lastSynchroTime masterFileInfos userFilesPath
  let mfsUpdate = [] -- getMasterFilesUpdate uTime uFileInfos lastSynchroTime masterFileInfos userFilesPath
  let ufsDelete = [] -- getUserFilesDelete uTime uFileInfos lastSynchroTime masterFileInfos userFilesPath
  let mfsDelete = [] -- getMasterFilesDelete uTime uFileInfos lastSynchroTime masterFileInfos userFilesPath

  putStrLn $ "master needs update for = \n" ++
    "userUpdates = " ++ (show ufsUpdate) -- ++ "\n" ++
    --"masterUpdates = " ++ (show mfsUpdate) ++ "\n" ++
    --"userDeletes = " ++ (show ufsDelete) ++ "\n" ++
    --"masterDeletes = " ++ (show mfsDelete)
    
  return (ufsUpdate, ufsDelete, mfsUpdate, mfsDelete)

-- TODO take the time delta in account, if ever not the same lag (e.g. online server)  
getUserFilesUpdate :: Time -> Files -> Maybe Time -> Files -> String -> Files 
getUserFilesUpdate uTime uFiles lastSynchroTime mFiles userFilesPath = filter pred uFiles
  where
    pred :: File -> Bool
    pred ufi = sieve ufi lastSynchroTime mFiles

    sieve :: File -> Maybe Time -> Files -> Bool
    sieve ufi lt mfis = res
      where
        mfi :: Maybe File
        mfi = findMasterFileInfo userFilesPath mfis ufi
        predModif :: File -> Bool
        predModif  = isUserMoreRecent FI._modifyTime lastSynchroTime ufi
        predChange :: File -> Bool
        predChange = isUserMoreRecent FI._changeTime lastSynchroTime ufi
        userModificationRecent :: Maybe Bool
        userModificationRecent = case mfi of Nothing -> Just True; _ -> fmap predModif mfi
        userChangeRecent :: Maybe Bool
        userChangeRecent = case mfi of Nothing -> Just True; _ -> fmap predChange mfi
        res = all ( == Just True ) [userModificationRecent, userChangeRecent]

getMasterFilesUpdate :: Time -> Files -> Maybe Time -> Files -> String -> Files
getMasterFilesUpdate uTime uFiles lastSynchroTime mFiles userFilesPath = filter pred mFiles
  where
    pred mfi = False

getUserFilesDelete :: Time -> Files -> Maybe Time -> Files -> String -> Files
getUserFilesDelete uTime uFiles lastSynchroTime mFiles userFilesPath = filter pred uFiles
  where
    pred ufi = False

getMasterFilesDelete :: Time -> Files -> Maybe Time -> Files -> String -> Files
getMasterFilesDelete uTime uFiles lastSynchroTime mFiles userFilesPath = filter pred mFiles
  where
    pred mfi = False
  
findMasterFileInfo :: String -> Files -> File -> Maybe File
findMasterFileInfo userFilesPath masterFileInfos uFileInfo = mfi
  where
    mfi = DL.find predicate masterFileInfos
    predicate fi = masterFileInfoPath == (view FI.filePath fi) 
    masterFileInfoPath = userFilesPath ++ "/" ++ (view FI.filePath uFileInfo)
    
isUserMoreRecent :: (FI.FileInfo -> String) -> Maybe Time -> File -> File -> Bool
isUserMoreRecent getter lastSynchroTime ufi mfi = isRecent || isMoreRecentThanLastSynchro
  where
    isRecent = (parseStringToDate $ getter ufi) < (parseStringToDate $ getter mfi)
    isMoreRecentThanLastSynchro = case lastSynchroTime of
                                    Nothing -> True
                                    Just t -> (parseStringToDate $ getter ufi) < t

-- TODO check if it is plateform dependent
parseStringToDate :: String -> Time
parseStringToDate (y1:y2:y3:y4:'-':m1:m2:'-':d1:d2:' ':h1:h2:':':mt1:mt2:':':s1:s2:rest) = utcTime
  where
    y = read (y1:y2:y3:y4:[]) :: Integer
    m = read (m1:m2:[]) :: Int
    d = read (d1:d2:[]) :: Int
    h = read (h1:h2:[]) :: Integer
    mt= read (mt1:mt2:[]) :: Integer
    s = read (s1:s2:[]) :: Integer
    day = DTCA.fromGregorian y m d
    nbSeconds :: Integer
    nbSeconds = s + 60 * mt + 3600 * h
    utcTime = DTC.UTCTime {utctDay = day, utctDayTime = DTC.secondsToDiffTime nbSeconds}
parseStringToDate s = error $ " could not parse date = " ++ s

updateUserFilesInBackup :: UserName -> FB.FileBinaries -> IO ()
updateUserFilesInBackup uname fbs = do
  dirpath <- getUserFilesPath uname
  CM.forM_ fbs (updateUserFile dirpath) 
  return ()

putGreen :: String -> IO () 
putGreen s =  putStrLn $ "\x1b[32m" ++ s ++ "\x1b[0m" 

putRed :: String -> IO ()
putRed s =  putStrLn $ "\x1b[31m" ++ s ++ "\x1b[0m"

putBlue :: String -> IO ()
putBlue s =  putStrLn $ "\x1b[34m" ++ s ++ "\x1b[0m"

updateUserFile :: String -> FB.FileBinary -> IO ()
updateUserFile pth (FB.FileBinary (FI.FileInfo configPth filePth modif chang) bytes) = do
  let entirePath = pth ++ configPth ++ "/" ++ filePth
  isExisting <- SD.doesFileExist entirePath
  -- create directory file is not exisiting yet
  CM.when (not isExisting) $ do
    let dirname = SFPP.takeDirectory entirePath
    SD.createDirectoryIfMissing True dirname    
    putGreen $ "created directory = " ++ dirname    
  -- saves file
  DBS.writeFile entirePath bytes
  putGreen $ "file saved = " ++ entirePath
  -- modifies the modification and change date to be the same as what the user sent
