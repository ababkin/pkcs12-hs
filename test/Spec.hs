import Crypto.Pkcs12.BmpString (
  BmpString(..),
  encode,
  decode
  )
import Data.ByteString
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "BmpString" $ do

    describe "encode" $ do
      it "encodes a String" $ do
        unpack (unBmpString (encode "Beavis")) `shouldBe`
          [0x00, 0x42, 0x00, 0x65, 0x00, 0x61, 0x00, 0x76, 0x00, 0x69, 0x00, 0x73, 0x00, 0x00]

    describe "decode" $ do
      it "decodes an encoded BmpString" $ property $
        \s -> decode (encode s) `shouldBe` Right s
