-- {-# LANGUAGE DatatypeContexts #-}
module CryptoPals.Analysis.Frequency where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Map        (fromListWith, toList)
import           Data.Word       (Word8)

-- data (Fractional f) => ByteFrequency f = ByteFrequency Word8 f
data ByteFrequency f = ByteFrequency Word8 f

bfWord :: Fractional f => ByteFrequency f -> Word8
bfWord (ByteFrequency w _) = w

bfFreq :: Fractional f => ByteFrequency f -> f
bfFreq (ByteFrequency _ f) = f

frequencies :: Fractional f => ByteString -> [ByteFrequency f]
frequencies xs        = fmap freq counts
    where counts      = toList $ fromListWith (+) [(c, 1) | c <- B.unpack xs]
          freq (x, c) = ByteFrequency x $ c / fromIntegral (B.length xs)

-- | https://en.wikipedia.org/wiki/Herfindahl_index
--   https://en.wikipedia.org/wiki/Diversity_index#Simpson_index
herfindahlIndex :: Fractional f => [f] -> f
herfindahlIndex = sum . fmap (^^ 2)

-- | https://en.wikipedia.org/wiki/Herfindahl_index
--   https://en.wikipedia.org/wiki/Diversity_index#Simpson_index
normalizedHerfindahlIndex :: (Integral i, Fractional f) => i -> [f] -> f
normalizedHerfindahlIndex k s = ((k' * herfindahlIndex s) - 1) / (k' - 1)
    where k' = fromIntegral k
