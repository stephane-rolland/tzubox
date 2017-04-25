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
  deriving (Show,Generic)

instance Binary FileBinary
instance NFData FileBinary

type FileBinaries = [FileBinary]


getFileBinary :: FileInfo -> IO FileBinary
getFileBinary fi = do
  let entirePath = (view configPath fi) ++ "/" ++ (view filePath fi)
  putStrLn $ "retrieve bytes for file = " ++ entirePath
  fileByteString <- DBS.readFile entirePath
  putStrLn $ "OK for = " ++ entirePath
  return $ FileBinary fi fileByteString
