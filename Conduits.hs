module Conduits where

import Control.Monad.IO.Class

import Data.Conduit
import qualified Data.Conduit.Util as CU
import qualified Data.Conduit.List as CL
import Data.Conduit.Binary
import Data.Conduit.Serialization.Binary
import Data.CSV.Conduit
import Data.List
import qualified Data.ByteString as B

import Text.Printf

import Types

mergeRows :: (Monad m) => Conduit [a] m a
mergeRows = CL.concat

applyFrom :: (Monad m) => [(a -> b)] -> Conduit a m b
applyFrom funcList = CL.scanl (\ a (f:fs) -> (fs, f a)) (cycle funcList)

fileToValues fileName =
  sourceFile fileName $=
  intoCSV defCSVSettings
  =$= mergeRows

fileToTypes fn = do
  as <- fileToValues fn =$= CL.map read =$= CL.consume
  cycleConduit as

cycleConduit as = CL.sourceList $ cycle as

printer ::
  (MonadIO m, MonadResource m, Show a) =>
  Conduit a m a
printer = do
  a <- await
  case a of
    Just val -> do
      liftIO . putStrLn . show $ val
      yield val
      printer
    Nothing -> return ()

showDValue :: DValue -> String
showDValue = printf "%d" . dNum

binaryToCSV end typs inFile outFile = 
  runResourceT $ 
  sourceFile inFile =$=
  conduitGet (sequence (map (getDValue end) typs)) =$=
  CL.map (map showDValue) =$=
  CL.map (intercalate ", ") =$=
  CL.map (++ "\n") =$=
  CL.map (B.pack . (map (toEnum . fromEnum))) $$
  sinkFile outFile

csvToBinary end typs inFile outFile =
  --keep reading number based on the types until the file ends.
  runResourceT $ 
  CU.zip (CL.sourceList (cycle typs)) (fileToValues inFile) =$=
  CL.map (putDValue end . (uncurry mkDValue)) =$=
  conduitPut $$
  sinkFile outFile

