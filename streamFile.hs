import Prelude as P

import GHC.IO.Handle
import System.IO as IO
import System.Environment
import System.Console.GetOpt

import Network.Socket

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import qualified Control.Concurrent as C

import Data.Conduit
import Data.Conduit.Binary as CB
import Data.Conduit.Network
import Data.Conduit.Filesystem
import qualified Data.ByteString as BS
import Data.Word


oneSecond = 1000000 

data Config = Config { monitoring :: Bool
                     , chunkSize :: Int
                     , sockType :: SocketType
                     }
defaultConfig = Config False (-1) Stream

pipeFileHandle hIn pos chunkSize monit = do
  size <- liftIO $ fromIntegral <$> hFileSize hIn
  let bytes = size - pos
  if (pos < size) && ((chunkSize <= 0) || (bytes >= chunkSize))
    then do
      let bytesToRead = if chunkSize <= 0 then bytes else chunkSize
      input <- liftIO $ BS.hGet hIn bytesToRead
      yield input
      pipeFileHandle hIn (pos + bytesToRead) chunkSize monit
  else if monit
     then do
      liftIO $ C.threadDelay oneSecond
      pipeFileHandle hIn pos chunkSize monit
     else return ()

allocSocket ip port typ = do
  socket <- socket AF_INET typ defaultProtocol
  hostAddr <- inet_addr ip
  connect socket $ SockAddrInet port hostAddr
  return socket
  
options :: [OptDescr (Config -> Config)]
options = 
 [
   Option ['c'] ["chunkSize"]
          (ReqArg (\ arg option -> option { chunkSize = read arg } )
                  "CHUNKSIZE")
          "Size of file chunks to read",

   Option ['m'] ["monitor"]
          (NoArg (\ option -> option {monitoring = True}))
          "Monitor the file and send newly written data over the network",

   Option ['u'] ["udp"]
          (NoArg (\ option -> option {sockType = Datagram}))
          "Send the file using UDP"
 ]
  

run (Config monit size typ) fileName ip port =  withSocketsDo $ runResourceT $ do
    (sockKey, sock) <- allocate (allocSocket ip port typ) sClose
    (hKey, h) <- allocate (IO.openFile fileName IO.ReadMode) hClose
    pipeFileHandle h 0 size monit $$ sinkSocket sock

main = do
  args <- getArgs
  case getOpt Permute options args of
    --Arguments correctly parsed
    (opts, inFile:ip:portStr:[],  [])   -> do
      --Make changes to default configuration
      let config = (foldl (.) id opts) defaultConfig 
      let port = fromIntegral $ (read portStr :: Int)
      --run program
      run config inFile ip port

    --Arguments not valid
    (_,     nonOpts, [])   -> error $
      "Unrecognized arguments: " ++ unwords nonOpts

    --Other errors
    (_,     _,       msgs) -> error $
      concat msgs ++ usageInfo "" options
