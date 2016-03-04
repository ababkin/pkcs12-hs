module Crypto.Pkcs12.Pbkdf (
  pbkdf
  ) where

import           Crypto.Pkcs12.BmpString (BmpString)
import qualified Data.ByteString as B
import           Data.Word (Word8)

-- https://tools.ietf.org/html/rfc7292#appendix-B.2
-- https://github.com/golang/crypto/blob/master/pkcs12/pbkdf.go

pbkdf :: (B.ByteString -> B.ByteString) ->
         Int ->
         Int ->
         B.ByteString ->
         BmpString ->
         Int ->
         Word8 ->
         Int ->
         B.ByteString
pbkdf hash u v salt password r id size = undefined
