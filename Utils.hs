module Utils where

import qualified Data.ByteString as B
import Data.List
import Data.List.Split
import Data.Word
import Data.Bits
import Data.Maybe
import Data.Char

digits = "0123456789ABCDEF"

-- Utility code
bs2hex :: B.ByteString -> String
bs2hex = concatMap toHex . map fromIntegral . B.unpack 

toHex n = (digits !! highByte) : (digits !! lowByte) : [] where
  highByte = (n `div` 16)
  lowByte  = (n `mod` 16)

hex2bs :: String -> B.ByteString
hex2bs = B.pack . map toByte . chunksOf 2 . map hexDigit . removeSpaces where
  toByte = int2word . packByte
removeSpaces = filter (not . isSpace)

packByte (h:l:[]) = (h `shiftL` 4) .|. l
packByte as = error $ "Expected two element list, got " ++ show as

hexDigit c = let c' = toUpper c in
  maybe (error $ "No parse of " ++ [c] ++ " in toNum")
        id
        (elemIndex c' digits)

int2word :: Int -> Word8
int2word = toEnum . fromEnum

char2word :: Char -> Word8
char2word = toEnum . fromEnum

word2char :: Word8 -> Char
word2char = toEnum . fromEnum

singleton a = [a]

a # f = f a

runEach = foldl (.) id
