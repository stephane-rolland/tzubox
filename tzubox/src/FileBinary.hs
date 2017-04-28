module FileBinary where
import FileInfo

import qualified Data.ByteString as DBS
import Data.Binary
import Control.DeepSeq
import GHC.Generics (Generic)

import Control.Lens

data FileBinary = FileBinary
  {
      _fileInfo :: FileInfo
    , _bytes :: DBS.ByteString
  }
  deriving (Generic)

instance Binary FileBinary
instance NFData FileBinary

type FileBinaries = [FileBinary]

getUserFileBinary :: FileInfo -> IO FileBinary
getUserFileBinary fi = do
  let entirePath = (view configPath fi) ++ "/" ++ (view filePath fi)
  putStrLn $ "retrieve bytes for file = " ++ entirePath
  fileByteString <- DBS.readFile entirePath
  putStrLn $ "OK for = " ++ entirePath
  return $ FileBinary fi fileByteString

instance Show FileBinary where
  show (FileBinary fi byt) = "FileBinary " ++ (show $  quot (DBS.length byt) 1000) ++ " KB - " ++ (show fi)   
