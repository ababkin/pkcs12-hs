{-# LANGUAGE OverloadedStrings #-}
module Crypto.Pkcs12.MacSpec where

import qualified Crypto.Pkcs12.BmpString as Bmp
import Crypto.Pkcs12.Mac (
  MacData(..),
  DigestInfo(..),
  verifyMac
  )
import Crypto.X509.Pkix (
  AlgorithmIdentifier(..)
  )
import qualified Data.ByteString.Base16 as H
import qualified Data.ByteString as B
import           Test.Hspec
import           Test.QuickCheck

-- https://github.com/golang/crypto/blob/master/pkcs12/mac_test.go

spec :: Spec
spec = do
  describe "Mac" $ do
    it "verifyMac" $ do
      let
        digest = hex "18203dff1e16f492f2afc891a9bad6ca9dee5193"
        macSalt = hex "0102030405060708"
        algoId = AlgorithmIdentifier [1,2,3] ""
        td = MacData (DigestInfo algoId digest) macSalt 2048
        message = hex "0b0c0d0e0f"
        Just password = Bmp.encode ""
      verifyMac td message password `shouldBe` True

hex :: B.ByteString -> B.ByteString
hex = fst . H.decode
