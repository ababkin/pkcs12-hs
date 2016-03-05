{-# LANGUAGE OverloadedStrings #-}

module Crypto.Pkcs12.PbkdfSpec where

import qualified Crypto.Hash.SHA1 as SHA1
import Crypto.Pkcs12.BmpString (
  BmpString(..),
  encode
  )
import Crypto.Pkcs12.Crypto
import           Crypto.Pkcs12.Pbkdf (pbkdf)
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = describe "Pbkdf" $ do

  -- Test from https://github.com/golang/crypto/blob/575fdbe86e5dd89229707ebec0575ce7d088a4a6/pkcs12/pbkdf_test.go#L24
  -- This test triggers a case where I_j (in step 6C) ends up with leading zero
  -- byte, meaning that len(Ijb) < v (leading zeros get stripped by big.Int).
  it "handles leading zeros" $ do
    let salt = "\xf3\x7e\x05\xb5\x18\x32\x4b\x4b"
        password = BmpString "\x00\x00"
        expected = "\x00\xf7\x59\xff\x47\xd1\x4d\xd0\x36\x65\xd5\x94\x3c\xb3\xc4\xa3\x9a\x25\x55\xc0\x2a\xed\x66\xe1"
    pbkdf SHA1.hash 20 64 salt password 2048 1 24 `shouldBe` expected

  it "works for long keys" $ do
    let
      salt = "\xff\xff\xff\xff\xff\xff\xff\xff"
      Just password = encode "sesame"
      key = pbeCipherDeriveKey ShaWithTripleDESCBC salt password 2048
      expected = "\x7c\xd9\xfd\x3e\x2b\x3b\xe7\x69\x1a\x44\xe3\xbe\xf0\xf9\xea\x0f\xb9\xb8\x97\xd4\xe3\x25\xd9\xd1"
    key `shouldBe` expected
