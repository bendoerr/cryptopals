{-# LANGUAGE OverloadedStrings #-}

-- | http://cryptopals.com/sets/2/challenges/11
module CryptoPals.Attack.EcbCbcOracle ( BlackBoxMode(..)
                                      , BlackBoxResult(..)
                                      , ecbCbcOracle
                                      , randomEcbCbcOracle
                                      , detectEcbCbcOracleMode
                                      ) where

import           Crypto.Random
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B

import           CryptoPals.Analysis.Block
import           CryptoPals.Cipher.AES
import           CryptoPals.Cipher.Types
import           CryptoPals.Padding
import           CryptoPals.Random

data BlackBoxMode = ECB
                  | CBC
                  deriving (Show, Eq)

data BlackBoxResult = BlackBoxResult { resultCiphertext :: ByteString
                                     , resultActualMode :: BlackBoxMode
                                     -- ^ For verification only
                                     }

                    | BlackBoxGuess  { resultCiphertext :: ByteString
                                     , resultGuessMode  :: BlackBoxMode
                                     , resultActualMode :: BlackBoxMode
                                     -- ^ For verification only
                                     }
                    deriving (Show)

randomKeyCipher :: MonadRandom random => random AES128
randomKeyCipher = (cipherInit :: ByteString -> AES128) <$> getRandomByteString 16

randomIV :: MonadRandom random => AES128 -> random (IV AES128)
randomIV cipher = makeIV cipher <$> getRandomByteString 16

randomMode :: MonadRandom random => random BlackBoxMode
randomMode = getRandomBool >>= \b -> if b then return ECB else return CBC

randomPadding :: MonadRandom random => ByteString -> random ByteString
randomPadding bs = rPaddingFront bs >>= rPaddingBack
    where
        rPaddingBack bs = fmap (\p -> B.append bs p) rPaddingBytes
        rPaddingFront bs = fmap (\p -> B.append p bs) rPaddingBytes
        rPaddingBytes = getRandomNumBetween 5 10 >>= getRandomByteString

-- | Creates an oracle that sometimes uses a ECB mode and sometimes uses a CBC
-- | mode to encrypt plaintexts using the given cipher.
ecbCbcOracle :: MonadRandom random => AES128 -> ByteString -> random BlackBoxResult
ecbCbcOracle cipher plaintext = do
        mode <- randomMode
        iv <- randomIV cipher
        plaintext' <- pkcs5padding <$> randomPadding plaintext
        return (ecbCbcOracle' mode iv plaintext')
    where
        ecbCbcOracle' ECB _  plaintext = BlackBoxResult (ecbEncrypt cipher plaintext) ECB
        ecbCbcOracle' CBC iv plaintext = BlackBoxResult (cbcEncrypt cipher iv plaintext) CBC

-- | Produces an 'ecbCbcOracle' with the key already initialized.
randomEcbCbcOracle :: MonadRandom random => random (ByteString -> random BlackBoxResult)
randomEcbCbcOracle = do
        cipher <- randomKeyCipher
        return (ecbCbcOracle cipher)

-- | Given an oracle function that sometimes uses ECB and sometimes uses CBC
-- | detect which is being used for some plaintext.
-- |
-- | We can do this detection by blowing up the plaintext a few times and using
-- | our previous 'usingEcbMode' function to check for repeated blocks.
detectEcbCbcOracleMode :: MonadRandom random => (ByteString -> random BlackBoxResult) -> ByteString -> random BlackBoxResult
detectEcbCbcOracleMode oracleFunc plaintext =
        let plaintext' = foldl (\a _ -> a `B.append` plaintext) "" [0..100]
         in do
            oracleResult <- oracleFunc plaintext'
            let ciphertext = resultCiphertext oracleResult
                mode = if (usingEcbMode ciphertext) then ECB else CBC
            return (BlackBoxGuess ciphertext mode (resultActualMode oracleResult))
