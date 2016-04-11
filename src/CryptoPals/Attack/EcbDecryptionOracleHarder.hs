{-# LANGUAGE OverloadedStrings #-}

-- | http://cryptopals.com/sets/2/challenges/14
module CryptoPals.Attack.EcbDecryptionOracleHarder ( ecbDecryptionOracleHarder
                                                   , randomEcbDecryptionOracleHarder
                                                   , decryptUnknownHarder
                                                   ) where

import           Crypto.Random
import           Data.ByteString                             (ByteString)
import qualified Data.ByteString                             as B
import qualified Data.List                                   as L

import           CryptoPals.Attack.EcbDecryptionOracleSimple (decryptUnknownSimple)
import           CryptoPals.Cipher.AES
import           CryptoPals.Cipher.Types
import           CryptoPals.Padding
import           CryptoPals.Random
import           CryptoPals.Utils

randomKeyCipher :: MonadRandom random => random AES128
randomKeyCipher = (cipherInit :: ByteString -> AES128) <$> getRandomByteString 16

ecbDecryptionOracleHarder :: AES128 -> ByteString -> ByteString -> ByteString -> ByteString
ecbDecryptionOracleHarder cipher randomPrefix unknownPostfix controlled =
        let plaintext = pkcs5padding $ randomPrefix `B.append` controlled `B.append` unknownPostfix
         in ecbEncrypt cipher plaintext

randomEcbDecryptionOracleHarder :: MonadRandom random => random (ByteString -> ByteString -> ByteString)
randomEcbDecryptionOracleHarder = do
        cipher <- randomKeyCipher
        prefixLen <- getRandomNumBetween 2 32
        prefix <- getRandomByteString prefixLen
        return (ecbDecryptionOracleHarder cipher prefix)

oraclePrefixBlockLength oracle =
        let ctA = oracle "a"
            ctB = oracle "b"
            len = length . takeWhile (uncurry (==)) $ B.zip ctA ctB
         in len + 16

oraclePrefixDeadLength oracle prefixBlockLength =
        let chunkIdx = prefixBlockLength `div` 16
            try n = let ctA = (chunk 16 $ oracle (B.replicate n 0x61)) !! chunkIdx
                        ctB = (chunk 16 $ oracle (B.replicate n 0x62)) !! chunkIdx
                    in ctA /= ctB
         in maybe 16 (\n -> n - 1) $ L.find (try) [1..16]

decryptUnknownHarder oracle =
        let prefixLen = oraclePrefixBlockLength oracle
            deadLen   = oraclePrefixDeadLength oracle prefixLen
            oracle'   = B.drop prefixLen . oracle . B.append (B.replicate deadLen 0x61)
         in decryptUnknownSimple oracle'


