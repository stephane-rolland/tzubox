module User where

import qualified Config as C
import qualified UserNetwork as UN

main :: IO ()
main = do
  userConfig <- C.readUserConfig
  putStrLn $ show userConfig
  
  -- connects regularily to master
  UN.main
