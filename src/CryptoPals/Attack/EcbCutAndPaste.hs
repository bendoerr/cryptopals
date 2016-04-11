{-# LANGUAGE OverloadedStrings #-}

-- | http://cryptopals.com/sets/2/challenges/13
module CryptoPals.Attack.EcbCutAndPaste ( encryptProfile
                                        , decryptProfile
                                        , cutAndPasteRole
                                        ) where

import           Crypto.Random
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B

import           CryptoPals.Cipher.AES
import           CryptoPals.Cipher.Types
import           CryptoPals.Padding
import           CryptoPals.Random
import           CryptoPals.Support.Cookie
import           CryptoPals.Support.Profile

encryptProfile :: AES128 -> Profile -> ByteString
encryptProfile cipher = ecbEncrypt cipher . pkcs5padding . encodeCookie

decryptProfile :: AES128 -> ByteString -> Profile
decryptProfile cipher = decodeCookie . unPkcsPadding . ecbDecrypt cipher

cutAndPasteRole :: (ByteString -> ByteString) -> ByteString -> ByteString
cutAndPasteRole encFunc targetRole =
        let ciphertext = encFunc magicEmail
            copyAndPaste = B.take 16 $ B.drop 16 ciphertext
         in B.take 48 ciphertext `B.append` copyAndPaste
    where
    -- using what we know, we can deduce this might be useful. Also that padding will be taken off as well.
    --  |email=|          |admin|           |   |&uid=10&role=|user            |
    --  |   6  |   10     |  5  |    11     | 3 |  13         |                |
    --  |                 |    copy         |                 |   paste        |
        magicEmail = (B.replicate 9 0x7a) `B.append` "@" `B.append` (pkcs5padding targetRole) `B.append` ".fu"
        roleLen = B.length targetRole
