module Crypto.X509.Pkix where

import Data.ASN1.OID (OID)
import Data.ByteString (ByteString)

-- https://golang.org/src/crypto/x509/pkix/pkix.go

data AlgorithmIdentifier = AlgorithmIdentifier OID ByteString
