-- |
-- Password based key deviation function
module Crypto.Pkcs12.Pbkdf (
  pbkdf
  ) where

import           Crypto.Pkcs12.BmpString (
  BmpString(..)
  )
import qualified Data.ByteString as B
import Data.ByteString.Split (
  chunksOf
  )
import           Data.Word (Word8)

-- https://github.com/golang/crypto/blob/master/pkcs12/pbkdf.go

type HashFunc = B.ByteString -> B.ByteString

-- |
-- Password based key deviation function.
--
-- https://tools.ietf.org/html/rfc7292#appendix-B.2
pbkdf :: HashFunc ->
         Int ->
         Int ->
         B.ByteString ->
         BmpString ->
         Int ->
         Word8 ->
         Int ->
         B.ByteString
pbkdf hash u v salt password r id size =
  B.take size $ eachIdx i B.empty c
  where
    d = B.replicate v id
    s = fillWithRepeats salt v
    p = fillWithRepeats (unBmpString password) v
    i = B.append s p
    c = (size + u - 1) `div` u
    eachIdx :: B.ByteString ->
               B.ByteString ->
               Int ->
               B.ByteString
    eachIdx i acc idx
      | idx <= 1 = nextAcc
      | otherwise = eachIdx nextI nextAcc (idx - 1)
      where
        ai = hashN hash r (B.append d i)
        nextAcc = B.append acc $ B.take 20 ai
        b = B.take v (repeatBs ((v `div` B.length ai) + 1) ai)
        bbi = bsToInteger b
        nextI = B.concat $ map partI $ chunksOf v i
        partI bs = paddedIjb
          where ij = bsToInteger bs
                ijb = takeRightBs v $ integerToBs $ ij + bbi + 1
                paddedIjb = B.append
                  (B.replicate (v - B.length ijb) 0)
                  ijb
                
takeRightBs :: Int -> B.ByteString -> B.ByteString
takeRightBs n xs = B.drop (B.length xs - n) xs

-- |
-- Interpret ByteString as an unsigned big endian Integer
bsToInteger :: B.ByteString -> Integer
bsToInteger = B.foldl (\acc n -> acc * 0x100 + fromIntegral n) 0

-- |
-- Encode an unsigned Integer to ByteString
integerToBs :: Integer -> B.ByteString
integerToBs n = go n B.empty
  where go n acc
          | n == 0 = acc
          | otherwise = go nextN nextAcc
          where nextN = n `div` 0x100
                modBs = B.singleton (fromIntegral (n `mod` 0x100))
                nextAcc = B.append modBs acc

hashN :: HashFunc -> Int -> B.ByteString -> B.ByteString
hashN hash n src
  | n > 0 = hashN hash (n - 1) (hash src)
  | otherwise = src

fillWithRepeats :: B.ByteString -> Int -> B.ByteString
fillWithRepeats pattern v =
  B.take outputLen (repeatBs n pattern)
  where
    patLen = B.length pattern
    outputLen = v * ((patLen + v - 1) `div` v)
    n = (outputLen + patLen - 1) `div` patLen

repeatBs :: Int -> B.ByteString -> B.ByteString
repeatBs n bs = B.concat (replicate n bs)
