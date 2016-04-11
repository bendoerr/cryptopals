{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
module CryptoPals.Support.Profile where

import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Builder   as BB
import qualified Data.ByteString.Char8     as B8
import qualified Data.ByteString.Lazy      as BL
import           Data.Map.Lazy             (Map)
import qualified Data.Map.Lazy             as M

import           CryptoPals.Support.Cookie

data Profile = Profile { profileEmail :: ByteString
                       , profileUid   :: Int
                       , profileRole  :: ByteString
                       } deriving (Show)

instance Cookieable Profile where
    encodeCookieImpl :: Profile -> Map ByteString ByteString
    encodeCookieImpl p = M.fromDistinctAscList [ ("email", profileEmail p)
                                               , ("uid", BL.toStrict . BB.toLazyByteString . BB.intDec $ profileUid p)
                                               , ("role", profileRole p)]
    decodeCookieImpl :: Map ByteString ByteString -> Profile
    decodeCookieImpl m = Profile (m M.! "email") (read . B8.unpack $ m M.! "uid") (m M.! "role")

profileFor :: ByteString -> Profile
profileFor email = let e = cookieFilter email in Profile e 10 "user"
