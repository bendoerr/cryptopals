module CryptoPals.HammingDistance where

import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Word       (Word8)

-- | Counts the number of /bits/ that are set in the byte for example the binary value __1010__ has two bits set.
numberOfSetBits :: Word8 -> Int
numberOfSetBits x
    | x == 0    = 0
    | otherwise = 1 + numberOfSetBits (x .&. (x - 1))

-- | Calculates the total number of bit substitutions needed to change one 'ByteString' into another. For example given
--   the value 11111010 /(0xFA)/ and 10010110 /(0x96)/ there are clearly four changes that would have to be made. We can
--   'xor' the two values to make it more clear 11111010 xor 10010110 = 1101100 and count the set bits to do this.
hammingDistance :: ByteString -> ByteString -> Int
hammingDistance a b
    | B.length a == B.length b = sum (map (\ (x, y) -> numberOfSetBits (xor x y)) (B.zip a b))
    | otherwise                    = error "Lengths of both strings must be equal."

-- | Finds the average normalized hamming distance over a set of 'ByteString's. To normalize the hamming distance is
--   divided by the length of the byte strings being compared.
avgHammingDistance :: [ByteString] -> Rational
avgHammingDistance xs = toRational ( fst $ foldl sumHam (0, tail xs) xs) / toRational (length xs - 1)
    where sumHam (acc, []) _ = (acc, [])
          sumHam (acc, rem) xs' = let dst = toRational (hammingDistance xs' $ head rem)
                                      len = toRational (B.length xs')
                                      normalizedDst = dst / len
                                  in (acc + normalizedDst, tail rem)
