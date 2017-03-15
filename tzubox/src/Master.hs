module Master where

import qualified Config as C

main :: IO ()
main = do
  masterConfig <- C.readMasterConfig
  putStrLn $ show masterConfig
