-- | A library to do stuff.
module Lib
    (
      main
    ) where

import qualified Config as C
import qualified Master as M
import qualified User as U

main :: IO ()
main = do
  isMaster <- C.isMaster
  if isMaster
  then M.main
  else U.main
  
