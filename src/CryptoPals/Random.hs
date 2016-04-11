module CryptoPals.Random where

import           Crypto.Number.Generate
import           Crypto.Random
import           Data.Bits              (shiftR)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B

getRandomByteString :: MonadRandom m => Int -> m ByteString
getRandomByteString length = getRandomBytes length

getRandomBool :: (MonadRandom m) => m Bool
getRandomBool = (toEnum :: Int -> Bool) . fromInteger <$> generateMax 2

getRandomNumBetween :: (MonadRandom m) => Int -> Int -> m Int
getRandomNumBetween mn mx = fromIntegral <$> generateBetween (fromIntegral mn) (fromIntegral mx)


