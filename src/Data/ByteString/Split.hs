module Data.ByteString.Split (
  chunksOf
  ) where

import qualified Data.ByteString as B

chunksOf :: Int -> B.ByteString -> [B.ByteString]
chunksOf n = go
  where
    go bs = case B.splitAt n bs of
      (a, b) | isEmpty a -> []
             | otherwise -> a : go b

isEmpty :: B.ByteString -> Bool
isEmpty bs = B.length bs == 0
