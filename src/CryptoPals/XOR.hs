module CryptoPals.XOR where

import           Data.Bits       (xor)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Word       (Word8)
import           Data.List       (sortBy)
{-import           Data.List.Sort       (takeEvery)-}

import CryptoPals.Evaluate
import CryptoPals.Convert
import CryptoPals.HammingDistance

xorFixed :: ByteString -> ByteString -> ByteString
xorFixed a b
    | (BS.length a) == (BS.length b) = BS.pack $ BS.zipWith xor a b
    | otherwise                      = error "length a != length b"

xorSingle :: Word8 -> ByteString -> ByteString
xorSingle x s = BS.map (xor x) s

xorRepeat :: ByteString -> ByteString -> ByteString
xorRepeat key s = BS.pack $ zipWith xor (cycle $ BS.unpack key) $ BS.unpack s

recoverXorSingle :: ByteString -> (Word8, ByteString)
recoverXorSingle = best' . allPossibleXorSingle

recoverXorSingle' :: ByteString -> (Word8, ByteString, Rational)
recoverXorSingle' = best'' . allPossibleXorSingle

findXorSingle :: [ByteString] -> (Word8, ByteString, Rational)
findXorSingle ss = best $ map (recoverXorSingle') ss
    where best = head . sortBy (\(_, _, s1) (_, _, s2) -> compare s2 s1)

allPossibleXorSingle :: ByteString -> [(Word8, ByteString)]
allPossibleXorSingle s = map (\x -> (x, xorSingle x s)) [0x00..0xFF]

guessKeySize :: Int -> Int -> ByteString -> [(Int, Rational)]
guessKeySize mink maxk bs = sortDist $ map (avgDist) [mink..maxk]
    where avgDist ksize = (ksize, avgHammingDistance $ chunk ksize bs)
          sortDist = sortBy (\(_, s1) (_, s2) -> compare s1 s2)

recoverXorRepeat keyLen ct = (key, pt)
    where key = BS.pack $ map (fst . recoverXorSingle) $ turn keyLen ct
          pt = xorRepeat key ct

chunk :: Int -> ByteString -> [ByteString]
chunk i b = map (BS.take i) $ (splitter b) (:) [] where
    splitter b _ n | BS.length b < i = n
    splitter b c n = b `c` splitter (BS.drop i b) c n

turn :: Int -> ByteString -> [ByteString]
turn i b = BS.transpose $ chunk i b

