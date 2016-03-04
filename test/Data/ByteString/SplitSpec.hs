{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.SplitSpec where

import qualified Data.ByteString as B
import           Data.ByteString.Split
import           Test.Hspec
import           Test.QuickCheck

spec = describe "Split" $ do
  describe "chunksOf" $ do
    it "splits" $ do
      chunksOf 2 "abc" `shouldBe` ["ab", "c"]

    it "is same as concat chunks" $ property $
      \(i, s) -> let bs = B.pack s in
      i > 0 ==> (B.concat . chunksOf i) bs `shouldBe` bs
