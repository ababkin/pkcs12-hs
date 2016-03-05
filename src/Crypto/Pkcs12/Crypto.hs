module Crypto.Pkcs12.Crypto (
  PbeCipher,
  ShaWithTripleDESCBC(..),
  ShaWith40BitRC2CBC(..),
  pbeCipherDeriveKey
  ) where

import qualified Data.ByteString as B
import qualified Crypto.Hash.SHA1 as SHA1
import Crypto.Pkcs12.BmpString (
  BmpString
  )
import Crypto.Pkcs12.Pbkdf (pbkdf)

-- https://github.com/golang/crypto/blob/master/pkcs12/crypto.go

-- |
-- PbeCipher is an abstraction of a PKCS#12 cipher.
class PbeCipher a where
  -- pbeCipherCreate :: a -> B.ByteString -> Either String Block
  pbeCipherDeriveKey :: a -> B.ByteString -> BmpString -> Int -> B.ByteString
  pbeCipherDeriveIV :: a -> B.ByteString -> BmpString -> Int -> B.ByteString

data ShaWithTripleDESCBC = ShaWithTripleDESCBC

instance PbeCipher ShaWithTripleDESCBC where
  pbeCipherDeriveKey _ salt password iterations =
    pbkdf SHA1.hash 20 64 salt password iterations 1 24
  pbeCipherDeriveIV _ salt password iterations =
    pbkdf SHA1.hash 20 64 salt password iterations 2 8

data ShaWith40BitRC2CBC = ShaWith40BitRC2CBC

instance PbeCipher ShaWith40BitRC2CBC where
  pbeCipherDeriveKey _ salt password iterations =
    pbkdf SHA1.hash 20 64 salt password iterations 1 5
  pbeCipherDeriveIV _ salt password iterations =
    pbkdf SHA1.hash 20 64 salt password iterations 2 8
