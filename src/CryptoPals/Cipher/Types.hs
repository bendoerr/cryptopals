-- | Taken from cryptonite (Crypto.Cipher.Types) but replaced with a more
-- manageable but maybe less "secure" ByteString interface.
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
module CryptoPals.Cipher.Types ( KeySizeSpecifier(..)
                               , Cipher(..)
                               , BlockCipher(..)
                               , BreakableBlockCipher(..)
                               , IV(..)
                               , makeIV
                               , nullIV) where

import           Crypto.Cipher.Types (KeySizeSpecifier (..))
import           Data.Bits           (xor)
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as B

import           CryptoPals.Padding
import           CryptoPals.Utils

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

    cbcEncrypt :: cipher -> IV cipher -> ByteString -> ByteString
    cbcEncrypt = cbcEncryptGeneric

    cbcDecrypt :: cipher -> IV cipher -> ByteString -> ByteString
    cbcDecrypt = cbcDecryptGeneric

class BlockCipher cipher => BreakableBlockCipher cipher where
    -- | Recover the key from ciphertext encrypted with ECB mode
    ecbBreak :: ByteString -> Maybe cipher


data IV cipher = IV ByteString

instance Eq (IV c) where
    (IV a) == (IV b) = a == b

makeIV :: BlockCipher cipher => cipher -> ByteString -> IV cipher
makeIV cipher b = toIV cipher
        where toIV :: BlockCipher c => c -> IV c
              toIV cipher
                | B.length b == sz = IV b
                | otherwise        = error "IV must == block size"
                where sz = blockSize cipher

nullIV :: BlockCipher cipher => cipher -> IV cipher
nullIV cipher = toIV cipher
    where toIV :: BlockCipher cipher => cipher -> IV cipher
          toIV cipher = IV (B.replicate (blockSize cipher) 0x00)

cbcEncryptGeneric :: BlockCipher cipher => cipher -> IV cipher -> ByteString -> ByteString
cbcEncryptGeneric cipher ivini input = mconcat . doEnc ivini $ chunk (blockSize cipher) input
  where doEnc _  []          = []
        doEnc (IV iv) (i:is) =
            let o = ecbEncrypt cipher $ zipXor iv i
             in o : doEnc (IV o) is

cbcDecryptGeneric :: BlockCipher cipher => cipher -> IV cipher -> ByteString -> ByteString
cbcDecryptGeneric cipher ivini input = mconcat . doDec ivini $ chunk (blockSize cipher) input
  where
        doDec _  []     = []
        doDec (IV iv) (i:is) =
            let o = zipXor iv $ ecbDecrypt cipher i
             in o : doDec (IV i) is
