module Config where

import qualified System.Directory as SD
import qualified System.IO as SIO
import qualified Data.List as DL
import qualified Data.Maybe as DM

pathConfigMaster :: String
pathConfigMaster = "/home/code/src/tzubox/simul/master/.master.cfg"

pathConfigUser :: String
pathConfigUser = "/home/code/src/tzubox/simul/user/.user.cfg"

data Path = Path
  {
    _directory :: String
  } deriving (Show,Read,Eq)

data Parsed =   ParsedPath Path
              | ParsedMasterIp MasterIp
              | ParsedBackupPath Path
              deriving (Show,Read,Eq)

type Paths = [Path]


data MasterIp = MasterIp
  {
    _ip :: String
  } deriving (Show,Read,Eq)

data UserConfig = UserConfig
  {
      _masterip :: MasterIp
    , _paths :: Paths
  } deriving (Show,Read,Eq)

data MasterConfig = MasterConfig
  {
     _backupPath :: Path
  } deriving (Show,Read,Eq)


isMaster :: IO (Bool)
isMaster = do
  isFileExists <- SD.doesFileExist pathConfigMaster
  return isFileExists
  
readUserConfig :: IO (UserConfig)
readUserConfig = do
  allLines <- SIO.readFile pathConfigUser
  let userConfig = parseUser $ lines $ allLines 
  return userConfig

readMasterConfig :: IO (MasterConfig)
readMasterConfig = do
  allLines <- SIO.readFile pathConfigMaster
  let masterConfig = parseMaster $ lines $ allLines
  return masterConfig


parseUser :: [String] -> UserConfig
parseUser [] = error "The user configuration file is empty"
parseUser ss = mkUserConfig $ DM.catMaybes $ map parseLine ss

parseMaster :: [String] -> MasterConfig
parseMaster [] = error "The master configuration file is empty"
parseMaster ss = mkMasterConfig $ DM.catMaybes $ map parseLine ss

mkUserConfig :: [Parsed] -> UserConfig
mkUserConfig prs = UserConfig ip ps
  where
    (ParsedMasterIp ip) = head $ filter (\case ParsedMasterIp _ -> True; _ -> False) prs 
    ps = foldr reducer [] prs

    reducer :: Parsed -> Paths -> Paths
    reducer (ParsedPath p) l = p : l
    reducer _ l = l

mkMasterConfig :: [Parsed] -> MasterConfig
mkMasterConfig prs = MasterConfig pth
  where
    (ParsedBackupPath pth) = head $ filter (\case ParsedBackupPath _ -> True; _ -> False) prs

    
parseLine :: String -> Maybe Parsed
parseLine s = maybeUserConfig
  where
    maybeUserConfig = parseWords $ words s

    parseWords :: [String] -> Maybe Parsed
    parseWords ("path" : "=" : args) = Just $ ParsedPath $ Path $ DL.intercalate " " args
    parseWords ("masterip" : "=" : args) = Just $ ParsedMasterIp $ MasterIp $ DL.intercalate " " args
    parseWords ("backup-path" : "=" : args) = Just $ ParsedBackupPath $ Path $ DL.intercalate " " args
    parseWords args = error $ "could not find path = in " ++ (DL.intercalate " " args)
    
