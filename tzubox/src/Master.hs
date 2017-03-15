module Master where

import qualified Config as C
import qualified MasterNetwork as MN

main :: IO ()
main = do
  masterConfig <- C.readMasterConfig
  putStrLn $ show masterConfig

  -- awaits connection from clients
  MN.main
  
