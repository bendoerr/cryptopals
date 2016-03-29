module CryptoPals.Convert where

import           Data.ByteString        (ByteString, pack, unpack)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
import           Data.Word              (Word8)

-- | Decodes a string of Hex or Base16 encoded form to bytes.
fromHex :: ByteString -> ByteString
fromHex = fst . Base16.decode

-- | Encodes into a string of bytes to Hex or Base16 form.
toHex :: ByteString -> ByteString
toHex = Base16.encode

-- | Decodes a string of Base64 encoded form to bytes.
fromBase64 :: ByteString -> ByteString
fromBase64 = Base64.decodeLenient

-- | Encodes into a string of bytes to Base64 form.
toBase64 :: ByteString -> ByteString
toBase64 = Base64.encode

-- | Alias for ByteString.pack.
fromDec :: [Word8] -> ByteString
fromDec = pack

-- | Alias for ByteString.unpack.
toDec :: ByteString -> [Word8]
toDec = unpack
