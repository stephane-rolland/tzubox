module UserConfig where

import qualified Data.List as DL
import qualified Data.Maybe as DM


data Path = Path
  {
    _directory :: String
  } deriving (Show,Read,Eq)

data MasterIp = MasterIp
  {
    _ip :: String
  } deriving (Show,Read,Eq)

type Paths = [Path]

data UserConfig = UserConfig
  {
      _masterip :: MasterIp
    , _paths :: Paths
  } deriving (Show,Read,Eq)


data Parsed =   ParsedPath Path
              | ParsedMasterIp MasterIp
              deriving (Show,Read,Eq)


parse :: [String] -> UserConfig
parse [] = UserConfig (MasterIp "") $ [] 
parse ss = mkUserConfig $ DM.catMaybes $ map parseLine ss

mkUserConfig :: [Parsed] -> UserConfig
mkUserConfig prs = UserConfig ip ps
  where
    (ParsedMasterIp ip) = head $ filter (\case ParsedMasterIp _ -> True; _ -> False) prs 
    ps = foldr reducer [] prs

    reducer :: Parsed -> Paths -> Paths
    reducer (ParsedPath p) l = p : l
    reducer _ l = l

    
parseLine :: String -> Maybe Parsed
parseLine s = maybeUserConfig
  where
    maybeUserConfig = parseWords $ words s

    parseWords :: [String] -> Maybe Parsed
    parseWords ("path" : "=" : args) = Just $ ParsedPath $ Path $ DL.intercalate " " args
    parseWords ("masterip" : "=" : args) = Just $ ParsedMasterIp $ MasterIp $ DL.intercalate " " args
    parseWords args = error $ "could not find path = in " ++ (DL.intercalate " " args)
    
