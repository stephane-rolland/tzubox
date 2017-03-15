-- | A library to do stuff.
module Lib
    (
      main
    ) where

import qualified Master as M
import qualified User as U
import qualified System.Environment as SE


main :: IO ()
main = do
  commandLineArgs <- SE.getArgs

  let isMaster = getIsMasterForced commandLineArgs

--  isMaster <- C.isMaster
  if isMaster
  then M.main
  else U.main
  
getIsMasterForced :: [String] -> Bool
getIsMasterForced args = (not . null $  args) && any (\x -> x == "master") args
