{-# LANGUAGE OverloadedStrings #-}

-- | http://cryptopals.com/sets/2/challenges/12
module CryptoPals.Attack.EcbDecryptionOracleSimple ( ecbDecryptionOracleSimple
                                                   , randomEcbDecryptionOracleSimple
                                                   , decryptUnknownSimple
                                                   ) where

import           Crypto.Random
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B
import qualified Data.List               as L
import           Data.Maybe              (fromMaybe)

import           CryptoPals.Cipher.AES
import           CryptoPals.Cipher.Types
import           CryptoPals.Padding
import           CryptoPals.Random

randomKeyCipher :: MonadRandom random => random AES128
randomKeyCipher = (cipherInit :: ByteString -> AES128) <$> getRandomByteString 16

ecbDecryptionOracleSimple :: AES128 -> ByteString -> ByteString -> ByteString
ecbDecryptionOracleSimple cipher unknown controlled =
        let plaintext = pkcs5padding $ B.append controlled unknown
         in ecbEncrypt cipher plaintext

randomEcbDecryptionOracleSimple :: MonadRandom random => random (ByteString -> ByteString -> ByteString)
randomEcbDecryptionOracleSimple = do
        cipher <- randomKeyCipher
        return (ecbDecryptionOracleSimple cipher)

decryptUnknownSimple :: (ByteString -> ByteString) -> ByteString
decryptUnknownSimple oracleFunc = unPkcsPadding $ foldl (decryptByte unknownLen) "" unknownRange
    where
        unknownLen = B.length (oracleFunc "")
        unknownRange = [unknownLen-1,unknownLen-2..0]
        decryptByte bs known n =
                let prefix = B.replicate n 0x41 `B.append` known
                    brutes = B.snoc prefix <$> [0x00..0xFF]
                    target = B.take bs $ oracleFunc (B.replicate n 0x41)
                    match nb = target == (B.take bs $ oracleFunc nb)
                 in B.drop n . fromMaybe prefix $ L.find match brutes

