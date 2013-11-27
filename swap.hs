import Prelude as P

import GHC.IO.Handle
import System.IO as IO
import System.Environment
import System.Console.GetOpt

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import qualified Control.Concurrent as C

import Data.Conduit
import Data.Conduit.Binary as CB
import Data.Conduit.Filesystem
import qualified Data.ByteString as BS
import Data.Word
import Data.Maybe


data SwapWidth = WordBoundary | DoubleWordBoundary

data Config = Config { swapWidth :: SwapWidth
                     , chunkSize :: Maybe Int
                     , outFileName :: String
                     }
defaultConfig = Config WordBoundary Nothing "output"

options :: [OptDescr (Config -> Config)]
options = 
 [
   Option ['c'] ["chunkSize"]
          (ReqArg (\ arg option -> option { chunkSize = read arg } )
                  "CHUNKSIZE")
          "Size of file chunks to read",

   Option ['o'] ["output"]
          (ReqArg (\ arg option -> option { outFileName = arg } )
                  "CHUNKSIZE")
          "Name of output file"

   Option ['w'] ["doublewidth"]
          (NoArg (\ option -> option {swapWidth = DoubleWordBoundary}))
          "Swap bytes on double word boundaries"
 ]
  
readFromHandle hIn pos chunkSize = do
  size <- liftIO $ fromIntegral <$> hFileSize hIn
  let bytes = size - pos
  if (pos < size)
    then do
      let bytesToRead = if isNothing chunkSize then bytes else min bytes chunkSize
      input <- liftIO $ BS.hGet hIn bytesToRead
      yield input
      pipeFileHandle hIn (pos + bytesToRead) chunkSize
  else return ()

swapper width = let
  bytes = case width of
    DoubleWordBoundary -> 4
    WordBoundary -> 2
  in do
    input <- await
    case input of
      Just bs -> 
      Nothing -> return ()
    

run (Config width size outFile) inFile = runResourceT $ do
    (outFile, hOut) <- allocate (IO.openFile inFile IO.WriteMode) hClose
    (inFile, hIn)   <- allocate (IO.openFile outFile IO.ReadMode) hClose
    readFromHandle hIn 0 size =$= swapper width $$ sinkFileHandle hOut

main = do
  args <- getArgs
  case getOpt Permute options args of
    --Arguments correctly parsed
    (opts, inFile:[],  [])   -> do
      --Make changes to default configuration
      let config = (foldl (.) id opts) defaultConfig 
      --run program
      run config inFile ip port

    --Arguments not valid
    (_,     nonOpts, [])   -> error $
      "Unrecognized arguments: " ++ unwords nonOpts

    --Other errors
    (_,     _,       msgs) -> error $
      concat msgs ++ usageInfo "" options
