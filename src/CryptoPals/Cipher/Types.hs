-- | Taken from cryptonite (Crypto.Cipher.Types) but replaced with a more
-- manageable but maybe less "secure" ByteString interface.
module CryptoPals.Cipher.Types ( KeySizeSpecifier(..)
                               , Cipher(..)
                               , BlockCipher(..)
                               , BreakableBlockCipher(..)) where

import           Crypto.Cipher.Types (KeySizeSpecifier (..))
import           Data.ByteString     (ByteString)

class Cipher cipher where
    -- | Initialize a cipher context from a key
    cipherInit    :: ByteString -> cipher
    -- | Cipher name
    cipherName    :: cipher -> String
    -- | return the size of the key required for this cipher.
    cipherKeySize :: cipher -> KeySizeSpecifier
    -- | return the key that this cipher is using
    cipherKey :: cipher -> ByteString


class Cipher cipher => BlockCipher cipher where
    -- | Return the size of block required for this block cipher
    blockSize    :: cipher -> Int

    -- | Encrypt blocks
    ecbEncrypt :: cipher -> ByteString -> ByteString

    -- | Decrypt blocks
    ecbDecrypt :: cipher -> ByteString -> ByteString

class BlockCipher cipher => BreakableBlockCipher cipher where
    -- | Recover the key from ciphertext encrypted with ECB mode
    ecbBreak :: ByteString -> Maybe cipher
