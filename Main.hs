module Main where

import Prelude as P hiding ((.), id)
import System.Console.GetOpt
import System.Environment
import System.FilePath.Posix

import Control.Applicative
import Control.Monad.IO.Class
import Control.Category

import qualified Data.ByteString as B
import qualified Data.Foldable as L
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.List
import Data.Conduit
import qualified Data.Conduit.Util as CU
import qualified Data.Conduit.List as CL
import Data.Conduit.Binary
import Data.Conduit.Serialization.Binary
import Data.CSV.Conduit

import Text.CSV
import Text.Printf


typs = [UB, UW, UD]
binFile = B.pack [1, 0, 2, 0, 0, 0, 3]
inputString = ["1", "2", "3"]

-- Data Types
data DType = UB | SB | UW | SW | UD | SD | UQ | SQ
  deriving (Show, Eq, Read)
data DValue = DInt { dType :: DType, dNum :: Int }

instance Show DValue where
  --show (DInt _ n) = show n
  show = printf "%d" . dNum

type Structure = [DType]

type FileName = String

data Endianess = BE | LE deriving (Show)

data Encoding = Binary | CSV

data Config = Config
  {
    endianess :: Endianess
  , types :: [DType]
  , coding :: Encoding
  , outputFile :: FileName
  }

defaultConfig = Config BE [UB] CSV "output"


structLength = sum . map typeLength 
typeLength UB = 1
typeLength SB = 1
typeLength UW = 2
typeLength SW = 2
typeLength UD = 4
typeLength SD = 4
typeLength UQ = 8
typeLength SQ = 8

ubyte = DInt UB . fromIntegral
sbyte = DInt SB . fromIntegral
uword = DInt UW . fromIntegral
sword = DInt SW . fromIntegral
udword = DInt UD . fromIntegral
sdword = DInt SD . fromIntegral
uqword = DInt UQ . fromIntegral
sqword = DInt SQ . fromIntegral

getDValue endian typ = case endian of
  BE -> case typ of
    UB -> ubyte  <$> getWord8
    SB -> sbyte  <$> getWord8
    UW -> uword  <$> getWord16be
    SW -> sword  <$> getWord16be
    UD -> udword <$> getWord32be
    SD -> sdword <$> getWord32be
    UQ -> uqword <$> getWord64be
    SQ -> sqword <$> getWord64be
  LE -> case typ of
    UB -> ubyte  <$> getWord8
    SB -> sbyte  <$> getWord8
    UW -> uword  <$> getWord16le
    SW -> sword  <$> getWord16le
    UD -> udword <$> getWord32le
    SD -> sdword <$> getWord32le
    UQ -> uqword <$> getWord64le
    SQ -> sqword <$> getWord64le
    

mkDValue :: DType -> String -> DValue
mkDValue t str = DInt t (read str)

putDValue endian typ = case endian of
  BE -> case typ of
    (DInt UB n) -> putWord8 $ fromIntegral n
    (DInt SB n) -> putWord8 $ fromIntegral n
    (DInt UW n) -> putWord16be $ fromIntegral n
    (DInt SW n) -> putWord16be $ fromIntegral n
    (DInt UD n) -> putWord32be $ fromIntegral n
    (DInt SD n) -> putWord32be $ fromIntegral n
    (DInt UQ n) -> putWord64be $ fromIntegral n
    (DInt SQ n) -> putWord64be $ fromIntegral n
  LE -> case typ of
    (DInt UB n) -> putWord8 $ fromIntegral n
    (DInt SB n) -> putWord8 $ fromIntegral n
    (DInt UW n) -> putWord16le $ fromIntegral n
    (DInt SW n) -> putWord16le $ fromIntegral n
    (DInt UD n) -> putWord32le $ fromIntegral n
    (DInt SD n) -> putWord32le $ fromIntegral n
    (DInt UQ n) -> putWord64le $ fromIntegral n
    (DInt SQ n) -> putWord64le $ fromIntegral n


-- Bit Data Definitions
data BitData = BDBit
             | BDMany Int BitData
             | BDSum BitData BitData

bit = BDBit
byte = BDMany 8 BDBit
word = BDMany 2 byte
doubleWord = BDMany 2 word

bdSize :: BitData -> Int
bdSize BDBit = 1
bdSize (BDMany n bd) = n * bdSize bd
bdSize (BDSum bd bd') = max (bdSize bd) (bdSize bd')

bdSum bd bd' = BDSum bd bd'
bd `fitsIn` bd' = bd `bdSum` bd'

-- Utility code
bs2hex :: B.ByteString -> String
bs2hex bs = concatMap toHex $ map fromIntegral $ B.unpack bs where
  digits = "0123456789ABCDEF"
  toHex n = (digits !! highByte) : (digits !! lowByte) : [] where
    highByte = (n `div` 16)
    lowByte  = (n `mod` 16)

-- 

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

num2byte = putWord8 . read

--Run main program, either encoding or decoding
run (Config end typs enc outFile) inFile =
  coding end inFile outFile where
    coding = case enc of
      CSV -> csvToBinary
      Binary -> binaryToCSV
  
csvToBinary end inFile outFile =
  --keep reading number based on the types until the file ends.
  runResourceT $ 
  CU.zip (CL.sourceList (cycle typs)) (fileToValues inFile) =$=
  CL.map (putDValue end . (uncurry mkDValue)) =$=
  conduitPut $$
  sinkFile outFile

singleton a = [a]

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

binaryToCSV end inFile outFile = 
  runResourceT $ 
  sourceFile inFile =$=
  conduitGet (sequence (map (getDValue end) typs)) =$=
  --CL.map (intercalate ", " . map show) =$=
  CL.map (map showDValue) =$=
  CL.map (intercalate ", ") =$=
  CL.map (++ "\n") =$=
  CL.map (B.pack . (map (toEnum . fromEnum))) $$
  sinkFile outFile
  

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
