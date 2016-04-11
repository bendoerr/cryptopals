module CryptoPals.Analysis.Block where

import           Control.Monad
import           Crypto.Random
import           Data.ByteString  (ByteString)
import qualified Data.ByteString  as B
import           Data.List        (find)
import           Data.Maybe       (fromJust)

import           CryptoPals.Utils

usingEcbMode :: ByteString -> Bool
usingEcbMode = hasRepeatedBlock 16

ecbBlockSize :: (ByteString -> ByteString) -> Int
ecbBlockSize oracle = fromJust $ find (\n -> hasRepeatedBlock n $ oracle' n) [2..256]
    where oracle' n = oracle $ B.replicate (n*2) 0x41

