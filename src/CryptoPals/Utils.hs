module CryptoPals.Utils where

import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as B
import qualified Data.Set            as Set

chunk :: Int -> ByteString -> [ByteString]
chunk i b = (B.take i) <$> (splitter b) [] where
    splitter b n | B.length b < i = n
    splitter b n = b : splitter (B.drop i b) n

transposeChunks :: Int -> ByteString -> [ByteString]
transposeChunks i = B.transpose . chunk i

hasRepeatedBlock :: Int -> ByteString -> Bool
hasRepeatedBlock blockSize bs = hasMatchingBlocks blocks
        where blocks = fmap (chunk blockSize) . take numBlocks $ B.tails bs
              hasMatchingBlocks = any (\xs -> length xs /= Set.size (Set.fromList xs))
              numBlocks = blockSize + (B.length bs `mod` blockSize) + 1
