module CryptoPals.Padding where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Maybe      (fromJust)
import           Data.Word       (Word8)

pkcs7padding :: Int -> ByteString -> ByteString
pkcs7padding multiple string = string `B.append` (B.replicate padLen (fromIntegral padLen))
    where padLen | modLen /= multiple  = multiple - modLen
                 | otherwise           = 0
          strLen = B.length string
          modLen = strLen `mod` multiple

unPkcsPadding :: ByteString -> ByteString
unPkcsPadding = fromJust . maybeUnPkcsPadding False

unPkcsPaddingStrict :: ByteString -> Maybe ByteString
unPkcsPaddingStrict = maybeUnPkcsPadding True

maybeUnPkcsPadding :: Bool -> ByteString -> Maybe ByteString
maybeUnPkcsPadding strict string =
    let last = B.last string
        amt  = fromIntegral last
        un = B.spanEnd ((==) last) string
        unl = B.length (snd un)
     in if (amt == unl) then Just (fst un) else (if (strict && amt < 16) then Nothing else Just (string))

pkcs5padding :: ByteString -> ByteString
pkcs5padding = pkcs7padding 16

padLeft :: Int -> Word8 -> ByteString -> ByteString
padLeft amount pad string = (B.replicate padLen pad) `B.append` string
    where padLen | strLen < amount = amount - strLen
                 | otherwise       = 0
          strLen = B.length string
