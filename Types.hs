module Types where

import Control.Applicative

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import Text.Printf

-- Data Types
data DType = UB | SB | UW | SW | UD | SD | UQ | SQ
  deriving (Show, Eq, Read)
data DValue = DInt { dType :: DType, dNum :: Int }

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

