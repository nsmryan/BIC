module Main where

import Prelude as P
import System.Console.GetOpt
import System.Environment
import System.FilePath.Posix

import Control.Monad.IO.Class

import Control.Applicative

import qualified Data.ByteString as BS
import Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit

import Utils

data Conversion = BinaryToHex | HexToBinary deriving (Eq)

data Config = Config { outFile :: String
                     , conversion :: Conversion}

defaultConfig = Config "output" HexToBinary

options :: [OptDescr (Config -> Config)]
options = 
 [
   Option ['b'] ["binary"]
          (NoArg (\ option -> option {conversion = BinaryToHex}))
          "convert binary to hex",

   Option ['o'] ["output"]
          (ReqArg (\ arg option -> option {outFile = arg})
                  "FILE")
          "output file name"
 ]
hexBinConduit = CL.map (hex2bs . map word2char . BS.unpack)
binHexConduit = CL.map (BS.pack . map char2word . bs2hex)

chunksOf n = CL.sequence (CB.take n)

printLength = do
  maybeBytes <- await
  case maybeBytes of
    Just bs -> do
      liftIO $ putStrLn $ "received bytes of length " ++ (show $ BS.length bs)
      yield bs
    Nothing -> return ()

run (Config outFile conv) inFile = runResourceT $
  sourceFile inFile =$=  printLength =$= converter $$ sinkFile outFile where
    converter = case conv of
      HexToBinary -> hexBinConduit
      BinaryToHex -> binHexConduit

main = do
  args <- getArgs
  case getOpt Permute options args of
    --Arguments correctly parsed
    (opts, inFile:[],  [])   -> do
      --Make changes to default configuration
      let config = runEach opts defaultConfig
      --run program
      run config inFile

    --Arguments not valid
    (_,     nonOpts, [])   -> error $
      "Unrecognized arguments: " ++ unwords nonOpts

    --Other errors
    (_,     _,       msgs) -> error $
      concat msgs ++ usageInfo "" options
