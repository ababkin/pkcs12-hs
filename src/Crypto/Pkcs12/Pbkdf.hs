module Crypto.Pkcs12.Pbkdf (
  pbkdf
  ) where

import qualified Data.ByteString as B
import Data.Word (Word8)

-- https://tools.ietf.org/html/rfc7292#appendix-B.2
-- https://github.com/golang/crypto/blob/master/pkcs12/pbkdf.go

pbkdf :: (B.ByteString -> B.ByteString) ->
         Int ->
         Int ->
         B.ByteString ->
         B.ByteString ->
         Int ->
         Word8 ->
         Int ->
         B.ByteString
pbkdf hash u v salt password r id size = undefined
