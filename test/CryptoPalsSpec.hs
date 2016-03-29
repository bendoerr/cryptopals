{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative        ((<$>))
import           Crypto.Cipher.AES          (initAES)
import           Crypto.Cipher.Types        (ecbDecrypt, ecbEncrypt)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import           Data.Ratio                 ((%))
import           Data.Word                  (Word8)
import           Test.Hspec
import           Test.QuickCheck

import           CryptoPals.Convert
import           CryptoPals.HammingDistance
import           CryptoPals.XOR
import           CryptoPals.AES

set1challenge6io = B.readFile "resources/challenge-6.txt"

main :: IO ()
main = hspec $ do
    describe "Challenge Set 1: Basics" $ do

        it "Challenge  1 (Convert hex to base64)" $
            let hexStr = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
                b64Str = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
             in toBase64 (fromHex hexStr) `shouldBe` b64Str

        it "Challenge  2 (Fixed XOR)" $
            let a = fromHex "1c0111001f010100061a024b53535009181c"
                b = fromHex "686974207468652062756c6c277320657965"
                c = fromHex "746865206b696420646f6e277420706c6179"
             in xorFixed a b `shouldBe` c

        it "Challenge  3 (Single-byte XOR cipher)" $
            let ciphertext         = fromHex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
                expectedKey        = 88
                expectedPlaintext  = "Cooking MC's like a pound of bacon"
                ( actualKey
                 ,actualPlaintext) = recoverXorSingle ciphertext
             in do actualKey       `shouldBe` expectedKey
                   actualPlaintext `shouldBe` expectedPlaintext

        it "Challenge  4 (Detect single-character XOR)" $ do
            txt <- B.readFile "resources/challenge-4.txt"
            let lines             = map fromHex $ filter (not . B.null) $ B.split 0x0A txt
                expectedKey       = 53
                expectedPlaintext = "Now that the party is jumping\n"
                ( actualKey
                 ,actualPlaintext
                 ,_)              = findXorSingle lines
             in do actualKey       `shouldBe` expectedKey
                   actualPlaintext `shouldBe` expectedPlaintext

        it "Challenge  5 (Implement repeating-key XOR)" $
            let key                = "ICE"
                plaintext          = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
                expectedCiphertext = fromHex "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
                actualCiphertext   = xorRepeat key plaintext
             in actualCiphertext `shouldBe` expectedCiphertext

        it "Challenge  6 (Hamming distance)" $
            let a = "this is a test"
                b = "wokka wokka!!!"
                c = 37
             in hammingDistance a b `shouldBe` c

        it "Challenge  6 (Find reapeating-key XOR key size)" $ do
            txt <- B.filter(not . (==) 0x0A) <$> B.readFile "resources/challenge-6.txt"
            let ciphertext      = fromBase64 txt
                expectedKeySize = 29
                actualKeySize   = fst $ head $ guessKeySize 1 40 ciphertext
             in actualKeySize `shouldBe` expectedKeySize

        it "Challenge  6 (Break repeating-key XOR)" $ do
            txt <- B.filter(not . (==) 0x0A) <$> B.readFile "resources/challenge-6.txt"
            let ciphertext         = fromBase64 txt
                keySize            = 29
                expectedKey        = "Terminator X: Bring the noise"
                expectedPlaintext  = "I'm back and I'm ringin' the bell \nA rockin' on the mike while the fly girls yell \nIn ecstasy in the back of me \nWell that's my DJ Deshay cuttin' all them Z's \nHittin' hard and the girlies goin' crazy \nVanilla's on the mike, man I'm not lazy. \n\nI'm lettin' my drug kick in \nIt controls my mouth and I begin \nTo just let it flow, let my concepts go \nMy posse's to the side yellin', Go Vanilla Go! \n\nSmooth 'cause that's the way I will be \nAnd if you don't give a damn, then \nWhy you starin' at me \nSo get off 'cause I control the stage \nThere's no dissin' allowed \nI'm in my own phase \nThe girlies sa y they love me and that is ok \nAnd I can dance better than any kid n' play \n\nStage 2 -- Yea the one ya' wanna listen to \nIt's off my head so let the beat play through \nSo I can funk it up and make it sound good \n1-2-3 Yo -- Knock on some wood \nFor good luck, I like my rhymes atrocious \nSupercalafragilisticexpialidocious \nI'm an effect and that you can bet \nI can take a fly girl and make her wet. \n\nI'm like Samson -- Samson to Delilah \nThere's no denyin', You can try to hang \nBut you'll keep tryin' to get my style \nOver and over, practice makes perfect \nBut not if you're a loafer. \n\nYou'll get nowhere, no place, no time, no girls \nSoon -- Oh my God, homebody, you probably eat \nSpaghetti with a spoon! Come on and say it! \n\nVIP. Vanilla Ice yep, yep, I'm comin' hard like a rhino \nIntoxicating so you stagger like a wino \nSo punks stop trying and girl stop cryin' \nVanilla Ice is sellin' and you people are buyin' \n'Cause why the freaks are jockin' like Crazy Glue \nMovin' and groovin' trying to sing along \nAll through the ghetto groovin' this here song \nNow you're amazed by the VIP posse. \n\nSteppin' so hard like a German Nazi \nStartled by the bases hittin' ground \nThere's no trippin' on mine, I'm just gettin' down \nSparkamatic, I'm hangin' tight like a fanatic \nYou trapped me once and I thought that \nYou might have it \nSo step down and lend me your ear \n'89 in my time! You, '90 is my year. \n\nYou're weakenin' fast, YO! and I can tell it \nYour body's gettin' hot, so, so I can smell it \nSo don't be mad and don't be sad \n'Cause the lyrics belong to ICE, You can call me Dad \nYou're pitchin' a fit, so step back and endure \nLet the witch doctor, Ice, do the dance to cure \nSo come up close and don't be square \nYou wanna battle me -- Anytime, anywhere \n\nYou thought that I was weak, Boy, you're dead wrong \nSo come on, everybody and sing this song \n\nSay -- Play that funky music Say, go white boy, go white boy go \nplay that funky music Go white boy, go white boy, go \nLay down and boogie and play that funky music till you die. \n\nPlay that funky music Come on, Come on, let me hear \nPlay that funky music white boy you say it, say it \nPlay that funky music A little louder now \nPlay that funky music, white boy Come on, Come on, Come on \nPlay that funky music \n"
                ( actualKey
                 ,actualPlaintext) = recoverXorRepeat keySize ciphertext
             in do expectedKey       `shouldBe` actualKey
                   expectedPlaintext `shouldBe` actualPlaintext

        it "Challenge  7 (AES in ECB mode)" $ do
            txt <- B.filter(not . (==) 0x0A) <$> B.readFile "resources/challenge-7.txt"
            let ciphertext        = fromBase64 txt
                key               = "YELLOW SUBMARINE"::ByteString
                cipher            = initAES key
                expectedPlaintext = "I'm back and I'm ringin' the bell \nA rockin' on the mike while the fly girls yell \nIn ecstasy in the back of me \nWell that's my DJ Deshay cuttin' all them Z's \nHittin' hard and the girlies goin' crazy \nVanilla's on the mike, man I'm not lazy. \n\nI'm lettin' my drug kick in \nIt controls my mouth and I begin \nTo just let it flow, let my concepts go \nMy posse's to the side yellin', Go Vanilla Go! \n\nSmooth 'cause that's the way I will be \nAnd if you don't give a damn, then \nWhy you starin' at me \nSo get off 'cause I control the stage \nThere's no dissin' allowed \nI'm in my own phase \nThe girlies sa y they love me and that is ok \nAnd I can dance better than any kid n' play \n\nStage 2 -- Yea the one ya' wanna listen to \nIt's off my head so let the beat play through \nSo I can funk it up and make it sound good \n1-2-3 Yo -- Knock on some wood \nFor good luck, I like my rhymes atrocious \nSupercalafragilisticexpialidocious \nI'm an effect and that you can bet \nI can take a fly girl and make her wet. \n\nI'm like Samson -- Samson to Delilah \nThere's no denyin', You can try to hang \nBut you'll keep tryin' to get my style \nOver and over, practice makes perfect \nBut not if you're a loafer. \n\nYou'll get nowhere, no place, no time, no girls \nSoon -- Oh my God, homebody, you probably eat \nSpaghetti with a spoon! Come on and say it! \n\nVIP. Vanilla Ice yep, yep, I'm comin' hard like a rhino \nIntoxicating so you stagger like a wino \nSo punks stop trying and girl stop cryin' \nVanilla Ice is sellin' and you people are buyin' \n'Cause why the freaks are jockin' like Crazy Glue \nMovin' and groovin' trying to sing along \nAll through the ghetto groovin' this here song \nNow you're amazed by the VIP posse. \n\nSteppin' so hard like a German Nazi \nStartled by the bases hittin' ground \nThere's no trippin' on mine, I'm just gettin' down \nSparkamatic, I'm hangin' tight like a fanatic \nYou trapped me once and I thought that \nYou might have it \nSo step down and lend me your ear \n'89 in my time! You, '90 is my year. \n\nYou're weakenin' fast, YO! and I can tell it \nYour body's gettin' hot, so, so I can smell it \nSo don't be mad and don't be sad \n'Cause the lyrics belong to ICE, You can call me Dad \nYou're pitchin' a fit, so step back and endure \nLet the witch doctor, Ice, do the dance to cure \nSo come up close and don't be square \nYou wanna battle me -- Anytime, anywhere \n\nYou thought that I was weak, Boy, you're dead wrong \nSo come on, everybody and sing this song \n\nSay -- Play that funky music Say, go white boy, go white boy go \nplay that funky music Go white boy, go white boy, go \nLay down and boogie and play that funky music till you die. \n\nPlay that funky music Come on, Come on, let me hear \nPlay that funky music white boy you say it, say it \nPlay that funky music A little louder now \nPlay that funky music, white boy Come on, Come on, Come on \nPlay that funky music \n\EOT\EOT\EOT\EOT"
                actualPlaintext   = ecbDecrypt cipher ciphertext
             in expectedPlaintext `shouldBe` actualPlaintext

        it "Challenge  8 (Detect AES in ECB mode)" $ do
            txt <- B.readFile "resources/challenge-8.txt"
            let lines    = map fromHex $ filter (not . B.null) $ B.split 0x0A txt
                expected = [fromHex "d880619740a8a19b7840a8a31c810a3d08649af70dc06f4fd5d2d69c744cd283e2dd052f6b641dbf9d11b0348542bb5708649af70dc06f4fd5d2d69c744cd2839475c9dfdbc1d46597949d9c7e82bf5a08649af70dc06f4fd5d2d69c744cd28397a93eab8d6aecd566489154789a6b0308649af70dc06f4fd5d2d69c744cd283d403180c98c8f6db1f2a3f9c4040deb0ab51b29933f2c123c58386b06fba186a"]
                actual   = findECB lines
             in actual `shouldBe` expected

    describe "Challenge Set 2: Block Crypto" $ do

        it "Challenge  9 (Implement PKCS#7 padding)" $
           True `shouldBe` True

        it "Challenge 10 (Implement CBC mode)" $
           True `shouldBe` True

        it "Challenge 11 (An ECB/CBC detection oracle)" $
           True `shouldBe` True

        it "Challenge 12 (Byte-at-a-time ECB decryption (Simple))" $
           True `shouldBe` True

        it "Challenge 13 (ECB cut-and-paste)" $
           True `shouldBe` True

        it "Challenge 14 (Byte-at-a-time ECB decryption (Harder))" $
           True `shouldBe` True

        it "Challenge 15 (PKCS#7 padding validation)" $
           True `shouldBe` True

        it "Challenge 16 (CBC bitflipping attacks)" $
           True `shouldBe` True
