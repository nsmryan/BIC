module Main where

import qualified Data.ByteString as B
import qualified Data.Foldable as L
import Data.Serialize

csv = [UB, UW, UD]
binFile = B.pack [1, 0, 2, 0, 0, 0, 3]
inputString = ["1", "2", "3"]

-- Data Types
data DType = UB | SB | UW | SW | UD | SD | UQW | SQW
newtype DValue = DInt DType Int

decodeData :: [DType] -> B.ByteString -> [DValue]
decodeData = undefined

mkDValue :: DType -> String -> DValue
mkDValue t str = DInt t (read str)

putDValue (DInt UB n) = putWord8 $ fromIntegral n
putDValue (DInt SB n) = putWord8 $ fromIntegral n
putDValue (DInt UW n) = putWord16be $ fromIntegral n
putDValue (DInt SW n) = putWord16be $ fromIntegral n
putDValue (DInt UD n) = putWord32be $ fromIntegral n
putDValue (DInt SD n) = putWord32be $ fromIntegral n
putDValue (DInt UQW n) = putWord64be $ fromIntegral n
putDValue (DInt SQW n) = putWord64be $ fromIntegral n

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

main = do
  print inputString
  print $ bs2hex binFile
  print (binFile == encodeData csv inputString)
