import Crypto.Pkcs12.BmpString (
  BmpString(..),
  encode,
  decode
  )
import qualified Data.ByteString as B
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "BmpString" $ do

    describe "encode" $ do
      it "encodes a String" $ do
        B.unpack (unBmpString (encode "Beavis")) `shouldBe`
          [0x00, 0x42, 0x00, 0x65, 0x00, 0x61, 0x00, 0x76, 0x00, 0x69, 0x00, 0x73, 0x00, 0x00]

      it "has (n + 1) * 2 bytes length" $ property $
        \s ->
        let (BmpString bs) = encode s in
        B.length bs `shouldBe` ((length s) + 1) * 2

      it "has 2 zeros at last" $ property $
        \s ->
        let (BmpString bs) = encode s in
        B.drop (B.length bs - 2) bs `shouldBe` B.pack [0, 0]

    describe "decode" $ do
      it "decodes an encoded BmpString" $ property $
        \s -> decode (encode s) `shouldBe` Right s

              
