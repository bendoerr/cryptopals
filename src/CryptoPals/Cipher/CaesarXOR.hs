module CryptoPals.Cipher.CaesarXOR (CaesarXOR(..), englishLikeDistributionSearch) where

import           Data.Bits                           (xor)
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString                     as B
import           Data.Word8                          (Word8)

import           CryptoPals.Analysis.LetterFrequency
import           CryptoPals.Cipher.Types

newtype CaesarXOR = CaesarXOR Word8 deriving (Show)

instance Cipher CaesarXOR where
    cipherName _ = "CaesarXOR"
    cipherInit = initCipher
    cipherKeySize _ = KeySizeFixed 1
    cipherKey = key

instance BlockCipher CaesarXOR where
    blockSize _ = 1
    ecbEncrypt  = encryptDecrypt
    ecbDecrypt  = encryptDecrypt

instance BreakableBlockCipher CaesarXOR where
    -- | Break the cipher for english plaintexts
    ecbBreak = bruteEnglishCaesar

key :: CaesarXOR -> ByteString
key (CaesarXOR k) = B.pack [k]

initCipher :: ByteString -> CaesarXOR
initCipher k
        | B.length k == 1 = CaesarXOR . head $ B.unpack k
        | otherwise       = error "key length != 1"

encryptDecrypt :: CaesarXOR -> ByteString -> ByteString
encryptDecrypt (CaesarXOR key) = B.map (xor key)

bruteEnglishCaesar :: ByteString -> Maybe CaesarXOR
bruteEnglishCaesar ciphertext = result $ evaluateBest allPossible
    where allPossible = fmap possible [0x00..0xFF]
          possible k = PlaintextPossibility k (B.map (xor k) ciphertext)
          result (Just (ScoredPossibility k _ _)) = Just (CaesarXOR k)
          result _ = Nothing

englishLikeDistributionSearch :: [ByteString] -> Maybe ByteString
englishLikeDistributionSearch bs = plaintext <$> evaluateDistributionBest allPossible
    where allPossible = fmap possible bs
          possible pt = PlaintextPossibility Nothing pt
