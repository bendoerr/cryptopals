-- {-# LANGUAGE DatatypeContexts #-}
module CryptoPals.Analysis.LetterFrequency ( evaluateAll
                                           , evaluateBest
                                           , evaluateDistributionAll
                                           , evaluateDistributionBest
                                           , PlaintextPossibility(..)
                                           ) where

-- module CryptoPals.Analysis.LetterFrequency where

import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as B
import           Data.List                     (maximumBy)
import           Data.Word8                    (Word8, isLower, toLower)

import           CryptoPals.Analysis.Frequency
import           CryptoPals.Analysis.Letters

data PlaintextPossibility key f = ScoredPossibility { possibleKey :: key, plaintext :: ByteString, score :: f}
                                | PlaintextPossibility { possibleKey :: key, plaintext :: ByteString } deriving (Show)

evaluateAll :: Fractional f => [PlaintextPossibility key f] -> [PlaintextPossibility key f]
evaluateAll = fmap calcScore

evaluateBest :: (Ord f, Fractional f) => [PlaintextPossibility key f] -> Maybe (PlaintextPossibility key f)
evaluateBest = bestPossibility . evaluateAll

evaluateDistributionAll :: Fractional f => [PlaintextPossibility key f] -> [PlaintextPossibility key f]
evaluateDistributionAll = fmap (\p -> ScoredPossibility (possibleKey p) (plaintext p) (scoreDistribution (plaintext p))) . filterRange

evaluateDistributionBest :: (Ord f, Fractional f) => [PlaintextPossibility key f] -> Maybe (PlaintextPossibility key f)
evaluateDistributionBest = bestPossibility . evaluateDistributionAll

bestPossibility :: (Ord f, Fractional f) => [PlaintextPossibility key f] -> Maybe (PlaintextPossibility key f)
bestPossibility = maxScore . filter gtZero
    where maxScore [] = Nothing
          maxScore xs = Just $ maximumBy comp xs

comp :: (Ord f, Fractional f) => PlaintextPossibility key f -> PlaintextPossibility key f -> Ordering
comp (ScoredPossibility _ _ s1) (ScoredPossibility _ _ s2) = compare s1 s2
comp (PlaintextPossibility _ _ ) (ScoredPossibility _ _ _) = GT
comp (ScoredPossibility _ _ _) (PlaintextPossibility _ _) = LT
comp (PlaintextPossibility _ _) (PlaintextPossibility _ _) = EQ

gtZero :: (Ord f, Fractional f) => PlaintextPossibility key f -> Bool
gtZero (ScoredPossibility _ _ s) = s > 0
gtZero (PlaintextPossibility _ _) = False

calcScore :: Fractional f => PlaintextPossibility key f -> PlaintextPossibility key f
calcScore p = result $ scorePrintable (plaintext p) * scoreLetters (plaintext p)
    where result s = ScoredPossibility (possibleKey p) (plaintext p) s

scoreDistribution :: Fractional f => ByteString -> f
scoreDistribution bs = 1 - (2 * abs (lcc - bd))
    where lcc = letterNormalizedHerfindahlIndex
          bd  = normalizedHerfindahlIndex numLetters $ bfFreq <$> frequencies bs

scoreLetters :: Fractional f => ByteString -> f
scoreLetters bs = adj * (1 - (2 * abs (lcc - cc)))
    where cc = normalizedHerfindahlIndex numLetters fqs
          lcc = letterNormalizedHerfindahlIndex
          lower = filterToLower bs
          fqs = bfFreq <$> frequencies lower
          adj = ll / tl
          ll = fromIntegral $ B.length lower
          tl = fromIntegral $ B.length bs

scorePrintable :: Fractional f => ByteString -> f
scorePrintable bs = (score ^^ 2) / (total ^^ 2)
    where score = fromIntegral $ B.foldl (accScore isPrintable) 0 bs
          total = fromIntegral $ B.length bs
          accScore f acc w = acc + fromEnum (f w)

filterRange :: Fractional f => [PlaintextPossibility key f] -> [PlaintextPossibility key f]
filterRange = filter inRange
    where range b = B.maximum b - B.minimum b
          inRange p = 116 >= (range $ plaintext p)
