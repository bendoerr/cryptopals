{-# LANGUAGE OverloadedStrings #-}

-- | http://cryptopals.com/sets/2/challenges/16
module CryptoPals.Attack.CbcBitFlipping where

import           Control.Arrow
import           Crypto.Random
import           Data.Bits               (xor)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B
import           Data.Map.Lazy           (Map)
import qualified Data.Map.Lazy           as M
import           Data.Maybe              (fromMaybe)

import           CryptoPals.Cipher.AES
import           CryptoPals.Cipher.Types
import           CryptoPals.Padding
import           CryptoPals.Random

encryptUserInfo :: AES128 -> IV AES128 -> ByteString -> ByteString
encryptUserInfo cipher iv userData =
        let dataPoints = M.fromDistinctAscList [ ("comment1", "cooking%20MCs")
                                               , ("userdata", filterUserData userData)
                                               , ("comment2", "%20like%20a%20pound%20of%20bacon") ]
        in cbcEncrypt cipher iv $ serialize dataPoints

decryptUserInfo :: AES128 -> IV AES128 -> ByteString -> Map ByteString ByteString
decryptUserInfo cipher iv = deserialize . cbcDecrypt cipher iv

userInfoIsAdmin userInfo = (fromMaybe "false" $ M.lookup "admin" userInfo) == "true"

serialize :: Map ByteString ByteString -> ByteString
serialize = B.intercalate ";" . M.foldrWithKey toPairs []
    where toPairs k v r = (k `B.append` "=" `B.append` v) : r

deserialize :: ByteString -> Map ByteString ByteString
deserialize = foldl handlePairs M.empty . splitToPairs
    where handlePairs r pair = let (pairKey, pairValue) = splitPair pair in M.insert pairKey pairValue r
          splitToPairs = B.split (w ';')
          splitPair = second B.tail . B.break (== w '=')
          w = fromIntegral . fromEnum

filterUserData = B.filter (\c -> c /= w ';' && c /= w '=')
    where w = fromIntegral . fromEnum

userInfoAttackStr = "000000000000000000000:admin+true" :: ByteString
userInfoFlipBits ciphertext =
        let mod1 = (B.index ciphertext 37) `xor` (w ';') `xor` (w ':')
            mod2 = (B.index ciphertext 43) `xor` (w '=') `xor` (w '+')
            w = fromIntegral . fromEnum
            split1 = B.take 37 ciphertext
            split2 = B.take 5 $ B.drop 38 ciphertext
            split3 = B.drop 44 ciphertext
         in B.concat [split1, (B.pack [mod1]), split2, (B.pack [mod2]), split3]

