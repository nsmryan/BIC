module Main where

import Prelude as P hiding ((.), id)
import System.Console.GetOpt
import System.Environment
import System.FilePath.Posix

import Control.Applicative
import Control.Category

import qualified Data.ByteString as B

import Text.CSV

import Types
import Conduits




--Run main program, either encoding or decoding
run (Config end typs enc outFile) inFile =
  coding end typs inFile outFile where
    coding = case enc of
      CSV -> csvToBinary
      Binary -> binaryToCSV
  

options :: [OptDescr (Config -> IO Config)]
options = 
 [
   Option ['c'] ["configFile"]
          (ReqArg (\ arg option -> do 
            contents <- parseCSVFromFile arg
            case contents of
              Left err -> error $ show err
              Right (typeNames:rest) -> do
                let typs = map read typeNames
                return $ option {types = typs})
                 "FILE")
          "configuration file",

   Option ['l'] ["little"]
          (NoArg (\ option -> return $ option {endianess = LE}))
          "uses little endian encoding instead of big endian",

   Option ['o'] ["output"]
          (ReqArg (\ arg option -> return $ option {outputFile = arg})
                  "FILE")
          "output file name",

   Option ['d'] ["coding"]
          (NoArg (\ option -> return $ option {coding = Binary}))
          "encode or decode"
 ]

main = do
  args <- getArgs
  case getOpt Permute options args of
    --Arguments correctly parsed
    (opts, inFile:[],  [])   -> do
      --Make changes to default configuration
      config <- foldl (>>=) (return defaultConfig) opts
      --run program
      run config inFile

    --Arguments not valid
    (_,     nonOpts, [])   -> error $
      "Unrecognized arguments: " ++ unwords nonOpts

    --Other errors
    (_,     _,       msgs) -> error $
      concat msgs ++ usageInfo "" options
