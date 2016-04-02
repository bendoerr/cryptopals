module CryptoPals.Cipher.VigenereXOR (VigenereXOR(..), guessKeySize) where

import           Data.Bits                           (xor)
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString                     as B
import           Data.List                           (minimumBy, sortBy)

import           CryptoPals.Analysis.HammingDistance
import           CryptoPals.Cipher.CaesarXOR
import           CryptoPals.Cipher.Types
import           CryptoPals.Utils

newtype VigenereXOR = VigenereXOR ByteString deriving (Show)

instance Cipher VigenereXOR where
    cipherName  _ = "VigenereXOR"
    cipherInit    = VigenereXOR
    cipherKeySize = KeySizeFixed . keySize
    cipherKey = key

instance BlockCipher VigenereXOR where
    blockSize = keySize
    ecbEncrypt = encryptDecrypt
    ecbDecrypt = encryptDecrypt

instance BreakableBlockCipher VigenereXOR where
    -- | Break the cipher for english plaintexts
    ecbBreak = breakVigenere

keySize :: VigenereXOR -> Int
keySize (VigenereXOR key) = B.length key

key :: VigenereXOR -> ByteString
key (VigenereXOR k) = k

encryptDecrypt :: VigenereXOR -> ByteString -> ByteString
encryptDecrypt (VigenereXOR keyBs) stringBs = B.pack $ zipWith xor key string
    where key = cycle $ B.unpack keyBs
          string = B.unpack stringBs

guessKeySize :: Int -> Int -> ByteString -> Int
guessKeySize mink maxk bs = fst . sortDist $ fmap (avgDist) [mink..maxk]
    where avgDist ksize = (ksize, avgHammingDistance $ chunk ksize bs)
          sortDist = minimumBy (\(_, s1) (_, s2) -> compare s1 s2)

breakVigenere ciphertext = VigenereXOR <$> key
    where keySize = guessKeySize 2 60 ciphertext
          key = foldl1 builder . fmap keyBytes $ transposeChunks keySize ciphertext
          keyBytes bs = cipherKey <$> (ecbBreak bs :: Maybe CaesarXOR)
          builder (Just a) (Just b) = Just $ B.append a b
          builder _ _ = Nothing
--           keyParts :: [Maybe ByteString]
--           keyParts = map recoverKeyByte $ transposeChunks keySize ciphertext
--           recoverKeyByte :: ByteString -> Maybe ByteString
--           escape (Just a) = a
--           escape Nothing = error "woops"

