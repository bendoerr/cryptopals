{-# LANGUAGE OverloadedStrings #-}
module CryptoPals.Support.Cookie  where

import           Control.Arrow
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Map.Lazy   (Map)
import qualified Data.Map.Lazy   as M

encodeCookieMap :: Map ByteString ByteString -> ByteString
encodeCookieMap = B.intercalate "&" . M.foldrWithKey toPairs []
    where toPairs k v r = (k `B.append` "=" `B.append` v) : r

decodeCookieMap :: ByteString -> Map ByteString ByteString
decodeCookieMap = foldl handlePairs M.empty . splitToPairs
    where handlePairs r pair = let (pairKey, pairValue) = splitPair pair in M.insert pairKey pairValue r

splitToPairs = B.split (w '&')
splitPair = second B.tail . B.break (== w '=')
w = fromIntegral . fromEnum

class Cookieable thing where
    encodeCookieImpl :: thing -> Map ByteString ByteString
    decodeCookieImpl :: Map ByteString ByteString -> thing

encodeCookie :: Cookieable thing => thing -> ByteString
encodeCookie = encodeCookieMap . encodeCookieImpl

decodeCookie :: Cookieable thing => ByteString -> thing
decodeCookie = decodeCookieImpl . decodeCookieMap

cookieFilter = B.filter (\c -> c /= w '&' && c /= w '=')
