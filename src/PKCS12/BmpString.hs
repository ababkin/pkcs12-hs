module PKCS12.BmpString (
  bmpString
  ) where

import Data.ByteString (ByteString)

-- https://github.com/golang/crypto/blob/master/pkcs12/bmp-string.go

bmpString :: String -> Either String ByteString
bmpString s = undefined
