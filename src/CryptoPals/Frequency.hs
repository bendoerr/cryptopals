module CryptoPals.Frequency where

import           Data.ByteString (ByteString, unpack)
import qualified Data.ByteString as B
import           Data.Map        (fromListWith, toList)
import           Data.Word       (Word8)

frequencies :: ByteString -> [(Word8, Rational)]
frequencies xs = map toP counts
    where counts     = toList $ fromListWith (+) [(c, 1) | c <- unpack xs]
          toP (x, c) = (x, c / fromIntegral (B.length xs))

frequencySumSquare :: [Rational] -> Rational
frequencySumSquare ps = sum $ map squareP ps
    where squareP p = p ^^ 2

concentrationCoefficient ki xs = ((k * frequencySumSquare ps) - 1) / (k - 1)
    where ps = map snd $ frequencies xs
          k = fromIntegral ki

concentrationCoefficient_ ki ps =
        ((k * frequencySumSquare ps) - 1) / (k - 1)
    where k = fromIntegral ki

letters = [0x61..0x79]
numLetters = 25

-- English Letters, Assumes Ascii/Latin Encoding
-- https://en.wikipedia.org/wiki/Letter_frequency
letterFrequency :: Word8 -> Rational
letterFrequency 0x61 = 0.08167 -- a
letterFrequency 0x62 = 0.01492 -- b
letterFrequency 0x63 = 0.02782 -- c
letterFrequency 0x64 = 0.04253 -- d
letterFrequency 0x65 = 0.12702 -- e
letterFrequency 0x66 = 0.02228 -- f
letterFrequency 0x67 = 0.02015 -- g
letterFrequency 0x68 = 0.06094 -- h
letterFrequency 0x69 = 0.06966 -- i
letterFrequency 0x6a = 0.00153 -- j
letterFrequency 0x6b = 0.00772 -- k
letterFrequency 0x6c = 0.04025 -- l
letterFrequency 0x6d = 0.02406 -- m
letterFrequency 0x6e = 0.06749 -- n
letterFrequency 0x6f = 0.07507 -- o
letterFrequency 0x70 = 0.01929 -- p
letterFrequency 0x71 = 0.00095 -- q
letterFrequency 0x72 = 0.05987 -- r
letterFrequency 0x73 = 0.06327 -- s
letterFrequency 0x74 = 0.09056 -- t
letterFrequency 0x75 = 0.02758 -- u
letterFrequency 0x76 = 0.00978 -- v
letterFrequency 0x77 = 0.02361 -- w
letterFrequency 0x78 = 0.01974 -- y
letterFrequency 0x79 = 0.00074 -- z

letterFrequencySquare :: Word8 -> Rational
letterFrequencySquare x = (letterFrequency x) ^^ 2

letterFrequencies :: [(Word8, Rational)]
letterFrequencies = map (\x -> (x, letterFrequency x)) letters

letterFrequencySumSquare :: Rational
letterFrequencySumSquare = frequencySumSquare $ map snd letterFrequencies

letterConcentrationCoefficient =
        ((k * letterFrequencySumSquare) - 1) / (k - 1)
    where k = fromIntegral numLetters
