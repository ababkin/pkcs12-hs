module Crypto.Pkcs12.Mac (
  MacData(..)
  )where

import Crypto.X509.Pkix (
  AlgorithmIdentifier
  )
import Crypto.Pkcs12.Pbkdf (pbkdf)
import qualified Data.ByteString as B

-- https://github.com/golang/crypto/blob/master/pkcs12/mac.go

data MacData = MacData DigestInfo B.ByteString Int

data DigestInfo = DigestInfo AlgorithmIdentifier B.ByteString

verifyMac :: MacData -> B.ByteString -> B.ByteString -> Maybe String
verifyMac macData message password = undefined
