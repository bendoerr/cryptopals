module CryptoPals.Evaluate where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.List       (sortBy)
import           Data.Word       (Word8)
import           Data.Word8      (isLower, isPrint, toLower)

import           CryptoPals.Frequency

score :: ByteString -> Rational
score bs = (scorePrintable bs) * (scoreLetters bs)

scoreAll :: [ByteString] -> [(ByteString, Rational)]
scoreAll = map (\b -> (b, score b))

scoreAll' :: [(t, ByteString)] -> [((t, ByteString), Rational)]
scoreAll' = map (\(k,b) -> ((k,b), score b))

best :: [ByteString] -> ByteString
best = fst . maxScore . scoreAll

best' :: [(t, ByteString)] -> (t, ByteString)
best' = fst . maxScore . scoreAll'

best'' :: [(t, ByteString)] -> (t, ByteString, Rational)
best'' = f . maxScore . scoreAll'
    where f ((k, b), s) = (k, b, s)

top'' :: [(t, ByteString)] -> [(t, ByteString, Rational)]
top'' = map f . topScores 10 . scoreAll'
    where f ((k, b), s) = (k, b, s)

bestScore = maxScore . scoreAll'

compareScore :: Ord a => (t, a) -> (t1, a) -> Ordering
compareScore (b1, s1) (b2, s2) = compare s2 s1

topScores :: Int -> [(t, Rational)] -> [(t, Rational)]
topScores n = take n .sortBy compareScore

maxScore :: [(t, Rational)] -> (t, Rational)
maxScore = head . sortBy compareScore

scorePrintable :: ByteString -> Rational
scorePrintable bs = (score ^^ 2) / (total ^^ 2)
    where score = toRational $ BS.foldl (accScore isPrintable) 0 bs
          total = toRational $ BS.length bs

scoreLetters bs = (*) adj $ (-) 1 $ (*) 2 $ abs (lcc - cc)
    where cc = concentrationCoefficient numLetters $ lower
          lcc = letterConcentrationCoefficient
          lower = filterToLower bs
          adj = ll / tl
          ll = toRational $ BS.length lower
          tl = toRational $ BS.length bs


accScore :: Enum a => (Word8 -> a) -> Int -> Word8 -> Int
accScore f acc w = (+) acc $ fromEnum $ f w

isPrintable :: Word8 -> Bool
isPrintable 0x0A = True -- new line
isPrintable 0x20 = True -- space
isPrintable w = (w >= 0x41 && w <= 0x5A) || (w >= 0x61 && w <= 0x7A) -- A-Z or a-z

filterToLower :: ByteString -> ByteString
filterToLower = BS.filter (isLower) . BS.map (toLower)
