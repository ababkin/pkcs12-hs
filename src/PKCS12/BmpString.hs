module PKCS12.BmpString (
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
encode :: String -> BmpString
encode s = BmpString bs
  where bsList = fmap (\c -> B.append
                             (B.singleton $ fromIntegral (ord c `div` 256))
                             (B.singleton $ fromIntegral (ord c `mod` 256)))
          (s ++ [chr 0])
        bs = foldl B.append B.empty bsList

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
