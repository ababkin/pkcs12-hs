module Crypto.Pkcs12.BmpString (
  BmpString(..),
  encode,
  decode
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.Char (ord, chr)
import           Data.List.Split (chunksOf)
import           Data.Word (Word8)

{-
References:
https://tools.ietf.org/html/rfc7292#appendix-B.1
http://en.wikipedia.org/wiki/Plane_(Unicode)#Basic_Multilingual_Plane
https://github.com/golang/crypto/blob/master/pkcs12/bmp-string.go
-}

newtype BmpString = BmpString { unBmpString :: B.ByteString }

-- |
-- BMP string encoding.
encode :: String -> Maybe BmpString
encode s =
  case all isBmp s of
    False -> Nothing
    True ->
      Just $ BmpString bs
      where bsList = fmap (\c -> B.append
                                 (bsFromInt (ord c `div` 0x100))
                                 (bsFromInt (ord c `mod` 0x100)))
              nullTerminated
            bs = foldl B.append B.empty bsList
            nullTerminated = s ++ ['\0']
            bsFromInt = B.singleton . fromIntegral

-- |
-- BMP string decoding.
decode :: BmpString -> Either String String
decode (BmpString bs) =
  case B.length bs `mod` 2 of
    0 ->
      Right $ fmap toChar (chunksOf 2 (dropRight 2 w8s))
      where w8s = B.unpack bs
            toChar :: [Word8] -> Char
            toChar [a, b] = chr (fromIntegral a * 256 + fromIntegral b)
    otherwise ->
      Left "odd-length BMP string"

dropRight :: Int -> [a] -> [a]
dropRight n xs = take (length xs - n) xs

-- |
-- Returns whether Char is in BMP (Basic Multilingual Plane).
--
-- BMP code point ranges 0000 to FFFF.
-- ref. https://en.wikipedia.org/wiki/Plane_(Unicode)#Basic_Multilingual_Plane
isBmp :: Char -> Bool
isBmp c = codePoint >= 0 && codePoint <= 0xFFFF
  where codePoint = ord c
