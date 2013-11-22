module Main where

import System.Console.GetOpt
import System.Environment
import System.FilePath.Posix

import Control.Applicative

import qualified Data.ByteString.Lazy as B
import qualified Data.Foldable as L
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.List
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Binary
import Data.Conduit.Serialization.Binary
import Data.CSV.Conduit


typs = [UB, UW, UD]
binFile = B.pack [1, 0, 2, 0, 0, 0, 3]
inputString = ["1", "2", "3"]

-- Data Types
data DType = UB | SB | UW | SW | UD | SD | UQ | SQ deriving (Show, Eq)
data DValue = DInt DType Int

instance Show DValue where
  show (DInt _ n) = show n

type Structure = [DType]

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

decodeData :: Structure -> B.ByteString -> Get String
decodeData typs bs = show <$> sequence $ map getDValue typs

getDValue UB = ubyte  <$> getWord8
getDValue SB = sbyte  <$> getWord8
getDValue UW = uword  <$> getWord16be
getDValue SW = sword  <$> getWord16be
getDValue UD = udword <$> getWord32be
getDValue SD = sdword <$> getWord32be
getDValue UQ = uqword <$> getWord64be
getDValue SQ = sqword <$> getWord64be

mkDValue :: DType -> String -> DValue
mkDValue t str = DInt t (read str)

putDValue (DInt UB n) = putWord8 $ fromIntegral n
putDValue (DInt SB n) = putWord8 $ fromIntegral n
putDValue (DInt UW n) = putWord16be $ fromIntegral n
putDValue (DInt SW n) = putWord16be $ fromIntegral n
putDValue (DInt UD n) = putWord32be $ fromIntegral n
putDValue (DInt SD n) = putWord32be $ fromIntegral n
putDValue (DInt UQ n) = putWord64be $ fromIntegral n
putDValue (DInt SQ n) = putWord64be $ fromIntegral n

encodeData :: Structure -> [String] -> Put
encodeData typs values = sequence_ . map putDValue $ zipWith mkDValue typs values

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
data Endianess = BE | LE deriving (Show)
type FileName = String
data Flag = FlagEndian String | FlagCfg FileName deriving (Show)
data Config = Config
  {
    endianess :: Endianess
  , types :: [DType]
  }

options :: [OptDescr Flag]
options = [ Option ['c'] ["configFile"] (ReqArg FlagCfg "FILE") "configuration file",
            Option ['e'] ["endianess"]  (ReqArg FlagEndian "BE") "endianess of files"
          ]

processConfig = undefined

processBinary config fileName = do
  bs <- B.readFile fileName
  let strs = decodeData [UB] bs
  let newFileName = addExtension ".bin" . dropExtension $ fileName
  writeFile newFileName (intercalate "," $ strs)

processText config fileName = do 
  strs <- filter (== ",") . words <$> readFile fileName
  let newFileName = addExtension ".csv" . dropExtension $ fileName
  let bs = encodeData [UB] strs
  B.writeFile newFileName bs

processFiles file flags = let config = processConfig flags in 
  if takeExtension file `elem` ["txt", "csv"]
    then processText config file
    else processBinary config file


mergeRows :: (Monad m) => Conduit [a] m a
mergeRows = CL.concat

main = do
  --args <- getArgs
  --case getOpt RequireOrder options args of
  --  (flags, [file],      [])   -> processFiles file flags
  --  (_,     nonOpts, [])   -> error $ "Unrecognized arguments: " ++ unwords nonOpts
  --  (_,     _,       msgs) -> error $ concat msgs ++ usageInfo "" options
  runResourceT $ do 
    put <- sourceFile "test.txt" $= intoCSV defCSVSettings $= mergeRows 
    typ <- mkDValue (cycle typs)
    conduitPut $$ sinkFile "test.bin"
  print $ (inputString == decodeData typs binFile)
  print $ (encodeData typs inputString == binFile)
  
