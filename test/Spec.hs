{-# LANGUAGE OverloadedStrings #-}

import Crypto.Pkcs12.BmpString (
  BmpString(..),
  encode,
  decode
  )
import qualified Data.ByteString as B
import Test.Hspec
import Test.QuickCheck
import qualified Data.ByteString.Base16 as H

main :: IO ()
main = hspec $ do
  describe "BmpString" $ do

    describe "encode" $ do
      it "encodes a String" $ do
        -- Example from https://tools.ietf.org/html/rfc7292#appendix-B.
        hexBmpString "Beavis" `shouldBe` Just "0042006500610076006900730000"
        -- Some characters from the "Letterlike Symbols Unicode block".
        hexBmpString "\x2115 - Double-struck N" `shouldBe`
          Just "21150020002d00200044006f00750062006c0065002d00730074007200750063006b0020004e0000"
        hexBmpString "\x0001f000 East wind (Mahjong)" `shouldBe` Nothing

      it "has (n + 1) * 2 bytes length" $ property $
        \s ->
        let Just (BmpString bs) = encode s in
        B.length bs `shouldBe` (length s + 1) * 2

      it "has 2 zeros at last" $ property $
        \s ->
        let Just (BmpString bs) = encode s in
        B.drop (B.length bs - 2) bs `shouldBe` B.pack [0, 0]

    describe "decode" $ do
      it "decodes an encoded BmpString" $ property $
        \s ->
        let Just bmp = encode s in
        decode bmp `shouldBe` Right s

hexBmpString :: String -> Maybe B.ByteString
hexBmpString s = fmap (H.encode . unBmpString) (encode s)
