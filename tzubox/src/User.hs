module User where

import qualified Config as C

main :: IO ()
main = do
  userConfig <- C.readUserConfig
  putStrLn $ show userConfig
  
