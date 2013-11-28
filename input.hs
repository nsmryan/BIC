{-# LANGUAGE OverloadedStrings #-}

import GHC.IO.Handle
import System.IO as IO

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Data.Conduit
import Data.Conduit.Filesystem
import Data.ByteString as BS

conduit h = do
    line <- liftIO $ IO.getLine
    liftIO $ IO.hPutStr h line
    liftIO $ hFlushAll h
    conduit h


main = do
  h <- openFile "test.bin" WriteMode
  runResourceT $ conduit h

