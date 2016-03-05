module Crypto.Pkcs12.Mac (
  MacData(..),
  DigestInfo(..),
  verifyMac
  )where

import Control.Exception (
  Exception,
  throw
  )
import Crypto.MAC.HMAC (
  hmac
  )
import qualified Crypto.Hash.SHA1 as SHA1
import Crypto.Pkcs12.BmpString (
  BmpString
  )
import           Crypto.Pkcs12.Pbkdf (pbkdf)
import Crypto.X509.Pkix (
  AlgorithmIdentifier(..)
  )
import           Data.ASN1.OID (OID)
import qualified Data.ByteString as B
import           Data.Typeable (Typeable)
import Debug.Trace

-- https://github.com/golang/crypto/blob/master/pkcs12/mac.go

data MacData = MacData DigestInfo B.ByteString Int

data DigestInfo = DigestInfo AlgorithmIdentifier B.ByteString

data Pkcs12Exception = NotImplemented String
                     deriving (Show, Typeable)
instance Exception Pkcs12Exception

oidSHA1 :: OID
oidSHA1 = [1, 3, 14, 3, 2, 26]

verifyMac :: MacData -> B.ByteString -> BmpString -> Bool
verifyMac macData message password =
  case algoOid of
    oidSHA1 ->
      (trace ("mac: " ++ show mac) mac) == (trace ("expected: " ++ show expectedMac) expectedMac)
      where key = pbkdf SHA1.hash 20 64 salt password iter 3 20
            blockSize = 64
            expectedMac = hmac SHA1.hash blockSize key message
    otherwise ->
      throw $ NotImplemented $ "unknown digest algorithm: " ++ show algoOid
  where
    MacData (DigestInfo (AlgorithmIdentifier algoOid _) mac) salt iter = macData

