import Prelude as P

import GHC.IO.Handle
import System.IO as IO
import System.Environment

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
import Data.ByteString as BS
import Data.Word

import Conduit


oneSecond = 1000000 

pipeFileHandle hIn pos chunkSize = do
  size <- liftIO $ fromIntegral <$> hFileSize hIn
  let bytes = size - pos
  if (pos < size) && ((chunkSize <= 0) || (bytes >= chunkSize))
    then do
      let bytesToRead = if chunkSize <= 0 then bytes else chunkSize
      input <- liftIO $ BS.hGet hIn bytesToRead
      yield input
      pipeFileHandle hIn (pos + bytesToRead) chunkSize
    else do
      liftIO $ C.threadDelay oneSecond
      pipeFileHandle hIn pos chunkSize

allocSocket ip port = do
  socket <- socket AF_INET Datagram defaultProtocol
  hostAddr <- inet_addr ip
  connect socket $ SockAddrInet port hostAddr
  return socket
  

main = do
  args <- getArgs
  let (fileName:ip:portStr:optional) = args
  let chunkSize = if P.null optional then (-1) else read . P.head $ optional
  mapM_ IO.putStrLn $
    [
      "fileName = " ++ fileName
    , "ip = " ++ ip
    , "port = " ++ portStr
    ]
  let port = fromIntegral $ (read portStr :: Int)
  withSocketsDo $ runResourceT $ do
    (sockKey, sock) <- allocate (allocSocket ip port) sClose
    (hKey, h) <- allocate (openH fileName) hClose
    pipeFileHandle h 0 chunkSize $$ sinkSocket sock
