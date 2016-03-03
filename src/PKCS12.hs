module PKCS12 (
              ) where

import Data.ASN1.OID (OID)
import Data.ByteString (ByteString)
import PKCS12.BmpString (
  encode
  )
import PKCS12.Mac (MacData)
import X509.Pkix (AlgorithmIdentifier)

-- ref. https://github.com/golang/crypto/blob/master/pkcs12/pkcs12.go

oidDataContentType = [1, 2, 840, 113549, 1, 7, 1]
oidEncryptedDataContentType = [1, 2, 840, 113549, 1, 7, 6]
oidFriendlyName = [1, 2, 840, 113549, 1, 9, 20]
oidLocalKeyID = [1, 2, 840, 113549, 1, 9, 21]
oidMicrosoftCSPName = [1, 3, 6, 1, 4, 1, 311, 17, 1]

data PfxPdu = PfxPdu Int ContentInfo MacData

data ContentInfo = ContentInfo OID ByteString

data EncryptedData = EncryptedData Int EncryptedContentInfo

data EncryptedContentInfo = EncryptedContentInfo OID AlgorithmIdentifier ByteString

data SafeBag = SafeBag OID ByteString [Pkcs12Attribute]

data Pkcs12Attribute = Pkcs12Attribute OID ByteString

data EncryptedPrivateKeyInfo = EncryptedPrivateKeyInfo AlgorithmIdentifier ByteString

data Block = Block -- TODO

toPEM :: ByteString -> Maybe String -> [Block]
toPEM pfxData password = undefined
