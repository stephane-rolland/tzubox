module UserConfig where

import qualified Data.List as DL
import qualified Data.Maybe as DM


data Path = Path
  {
    _directory :: String
  } deriving (Show,Read)

type Paths = [Path]

data UserConfig = UserConfig
  {
    _paths :: Paths
  } deriving (Show,Read)






parse :: [String] -> UserConfig
parse [] = UserConfig []
parse ss = UserConfig $ DM.catMaybes $ map parseLine ss

parseLine :: String -> Maybe Path
parseLine s = maybeUserConfig
  where
    maybeUserConfig = parseWords $ words s

    parseWords :: [String] -> Maybe Path
    parseWords ("path" : "=" : args) = Just $ Path $ DL.intercalate " " args
    parseWords args = error $ "could not find path = in " ++ (DL.intercalate " " args)
    
