module CryptoPals.AES where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Set as Set

import CryptoPals.XOR (chunk)

blockSize = 16

looksLikeECB :: ByteString -> Bool
looksLikeECB bs = hasMatchingBlocks blocks
        where blocks = map (chunk blockSize) $ take numBlocks $ B.tails bs
              hasMatchingBlocks = any (\xs -> length xs /= Set.size (Set.fromList xs))
              numBlocks = blockSize + (B.length bs `mod` blockSize) + 1

findECB :: [ByteString] -> [ByteString]
findECB = map snd . filter fst . map (\bs -> (looksLikeECB bs, bs))
