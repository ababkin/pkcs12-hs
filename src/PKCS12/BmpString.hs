module PKCS12.BmpString (
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

-- |
-- BMP string encoding.
encode :: String -> B.ByteString
encode s = B.append folded null
  where null = B.append (B.singleton 0) (B.singleton 0)
        bsList = fmap (\c -> B.append
                             (B.singleton $ fromIntegral (ord c `div` 256))
                             (B.singleton $ fromIntegral (ord c `mod` 256))) s
        folded = foldl B.append B.empty bsList

-- |
-- BMP string decoding.
decode :: B.ByteString -> Either String String
decode bs =
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
