module CryptoPals.Cipher.OneTimePad (OneTimePad(..)) where

import           Data.Bits               (xor)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B

import           CryptoPals.Cipher.Types

newtype OneTimePad = OneTimePad ByteString

instance Cipher OneTimePad where
    cipherName  _ = "OneTimePad"
    cipherInit    = OneTimePad
    cipherKeySize = KeySizeFixed . keySize
    cipherKey = key

instance BlockCipher OneTimePad where
    blockSize = keySize
    ecbEncrypt = encryptDecrypt
    ecbDecrypt = encryptDecrypt

keySize :: OneTimePad -> Int
keySize (OneTimePad key) = B.length key

key :: OneTimePad -> ByteString
key (OneTimePad k) = k

encryptDecrypt :: OneTimePad -> ByteString -> ByteString
encryptDecrypt (OneTimePad key) string
    | (B.length key) == (B.length string) = B.pack $ B.zipWith xor key string
    | otherwise                            = error "length key != length string"
