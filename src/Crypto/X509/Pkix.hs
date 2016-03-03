module Crypto.X509.Pkix where

import Data.ASN1.OID (OID)
import Data.ByteString (ByteString)

-- https://golang.org/pkg/crypto/x509/pkix/#AlgorithmIdentifier

data AlgorithmIdentifier = AlgorithmIdentifier OID ByteString
