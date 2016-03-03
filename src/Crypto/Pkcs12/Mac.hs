module Crypto.Pkcs12.Mac where

import Crypto.X509.Pkix (
  AlgorithmIdentifier
  )
import Data.ByteString (ByteString)

-- https://github.com/golang/crypto/blob/master/pkcs12/mac.go

data MacData = MacData DigestInfo ByteString Int

data DigestInfo = DigestInfo AlgorithmIdentifier ByteString
