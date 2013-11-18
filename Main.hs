module Main where

import System.Console.GetOpt
import System.Environment
import System.FilePath.Posix

import Control.Applicative

import qualified Data.ByteString as B
import qualified Data.Foldable as L
import Data.Serialize
import Data.List


csv = [UB, UW, UD]
binFile = B.pack [1, 0, 2, 0, 0, 0, 3]
inputString = ["1", "2", "3"]

-- Data Types
data DType = UB | SB | UW | SW | UD | SD | UQ | SQ
data DValue = DInt DType Int

instance Show DValue where
  show (DInt _ n) = show n

ubyte = DInt UB . fromIntegral
sbyte = DInt SB . fromIntegral
uword = DInt UW . fromIntegral
sword = DInt SW . fromIntegral
udword = DInt UD . fromIntegral
sdword = DInt SD . fromIntegral
uqword = DInt UQ . fromIntegral
sqword = DInt SQ . fromIntegral

decodeData :: [DType] -> B.ByteString -> [String]
decodeData typs bs = let dvalues = sequence $ map getDValue typs in
  case (runGet dvalues bs) of
    Left err -> error err
    Right as -> map show as

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

encodeData :: [DType] -> [String] -> B.ByteString
encodeData typs values = runPut . sequence_ . map putDValue $ zipWith mkDValue typs values

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
  let strs = decodeData (repeat UB) bs
  let newFileName = addExtension ".bin" . dropExtension $ fileName
  writeFile newFileName (intercalate "," $ strs)

processText config fileName = do 
  strs <- filter (== ",") . words <$> readFile fileName
  let newFileName = addExtension ".csv" . dropExtension $ fileName
  let bs = encodeData (repeat UB) strs
  B.writeFile newFileName bs

processFiles file flags = let config = processConfig flags in 
  if takeExtension file `elem` ["txt", "csv"]
    then processText config file
    else processBinary config file

main = do
  args <- getArgs
  case getOpt RequireOrder options args of
    (flags, [file],      [])   -> processFiles file flags
    (_,     nonOpts, [])   -> error $ "Unrecognized arguments: " ++ unwords nonOpts
    (_,     _,       msgs) -> error $ concat msgs ++ usageInfo "" options

