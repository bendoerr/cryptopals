module CryptoPals.Cipher.AES (AES128(..)) where

import           Control.Monad
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B

import qualified Crypto.Cipher.AES       as Crypto
import qualified Crypto.Cipher.Types     as Crypto
import qualified Crypto.Error            as Crypto

import           CryptoPals.Cipher.Types

data AES128 = AES128 Crypto.AES128 ByteString

instance Cipher AES128 where
    cipherName (AES128 internal _) = Crypto.cipherName internal
    cipherInit k = AES128 (unwrappedInit k) k
    cipherKeySize (AES128 internal _) = Crypto.cipherKeySize internal
    cipherKey (AES128 _ k) = k

instance BlockCipher AES128 where
    blockSize (AES128 internal _) = Crypto.blockSize internal
    ecbEncrypt (AES128 internal _) = Crypto.ecbEncrypt internal
    ecbDecrypt (AES128 internal _) = Crypto.ecbDecrypt internal

unwrappedInit :: ByteString -> Crypto.AES128
unwrappedInit keyBs = Crypto.throwCryptoError $ Crypto.cipherInit keyBs
