{-# LANGUAGE OverloadedStrings #-}

module Algo.CryptoPal where

import Algo.CryptoPalInput
import Data.ByteString.Base64 (encode, decode)
import Data.Digits
import Data.Char
import Data.Hex
import Data.String
import qualified Data.ByteString.Char8 as BT (unpack, pack)
import Numeric (showHex, readHex, showIntAtBase)
import Data.List.Split
import Data.Char
import qualified Data.Text as T (pack)
import Data.Bits
import Data.List
import Data.Function
import Data.Tuple.Extra
import qualified Data.Map as M
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text.Metrics (hamming)
import GHC.Utils.Misc (fstOf3, sndOf3, thdOf3)
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error
import Control.Applicative
import Data.Composition

toHex c2 = let [(n, _)] = readHex c2 in n

fromHexWith :: (Int -> a) -> String -> [a]
fromHexWith f xs = (f . toHex) <$> chunksOf 2 xs

hex2chars = fromHexWith chr
hex2ints = fromHexWith id

solve1 :: String -> String
solve1 = BT.unpack . encode . fromString . hex2chars

solved1 = solve1 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" == "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

fixedOr :: String -> String -> String
fixedOr xs ys = 
    let xored = (zipWith xor `on` hex2ints) xs ys
    in concatMap (flip showHex "") xored

solved2 = fixedOr "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965" == "746865206b696420646f6e277420706c6179"

decodeXor keys x = fromHexWith (xor (ord x)) keys

toPrintable c = if isPrint c || isSpace c then Just c else Nothing

aside f x = (,) <*> f $ x

score :: [Char] -> Double
score xs = sum $ (\x -> M.findWithDefault 0 (toUpper x) freq) <$> xs 

singleByteCipher :: [Char] -> String -> Maybe (Char, String, Double)
singleByteCipher keys xs =
    let scored = catMaybes $ (try1 <$> keys)
        sorted = sortBy (flip (compare `on` thdOf3)) scored
    in case sorted of
        [] -> Nothing
        (x:_) -> Just x
    where
        try1 k = 
            case traverse (toPrintable . chr) $ decodeXor xs k of
                Nothing -> Nothing
                Just r -> Just (k, r, score r)

singleByteAlpha = singleByteCipher ['A'..'z']

solved3 =
    let Just (_, expected, _) = singleByteAlpha "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    in "Cooking MC's like a pound of bacon" == expected

singleByteXor = singleByteCipher (chr <$> [0..255])

detectXor xs = 
    let singles = singleByteXor <$> xs
    in maximumBy (compare `on` sndOf3) <$> singles

pad :: Int -> a -> [a] -> [a]
pad n p xs = take (n - length xs) (repeat p ) ++ xs

ord2hex = pad 2 '0' . flip showHex ""
char2hex = ord2hex . ord

str2hex :: String -> String
str2hex = concatMap char2hex

bytes2hex :: [Int] -> String
bytes2hex = concatMap ord2hex

-- repeatingKeyXor :: String -> String -> String
repeatingKeyXor xs k = 
    bytes2hex $ zipWith (xor `on` ord) (concat $ repeat k) xs

solved5 = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f" == repeatingKeyXor "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal" "ICE"

int2bits ns = pad 8 '0' $ showIntAtBase 2 intToDigit ns ""
str2bits = concatMap (int2bits . ord)

hamming' :: String -> String -> Maybe Int
hamming' = hamming `on` (T.pack . str2bits)

keyWeight keySize = 
    let all4 = take 4 $ chunksOf keySize cypherText
        hms = catMaybes [ hamming' xs ys | xs <- all4, ys <- all4, xs /= ys ]
    in (fromInteger $ toInteger $ sum hms) / (fromInteger $ toInteger $ length hms) / (fromInteger $ toInteger keySize)

possibleKeySizes = 
    let weights = aside keyWeight <$> [1..40]
    in take 5 $ sortOn snd weights

tryFindKeys keySizes = 
    let chunks = transpose $ chunksOf keySizes cypherText
        candidates = singleByteXor . str2hex <$> chunks
        results = sequence $ (fmap fstOf3 <$> candidates)
    in results

breakRepeatingKeyXor keySizes = 
    case catMaybes $ tryFindKeys <$> keySizes of
        [] -> Nothing
        (key:_) -> Just $ fromHexWith chr $ repeatingKeyXor cypherText key    

aesEcbDecode :: String -> String -> String
aesEcbDecode key txt = BT.unpack $ ecbDecrypt cipher (BT.pack txt)
    where 
        cipher :: AES128
        cipher = let (CryptoPassed c) = cipherInit (BT.pack key) in c

aesEcbDecodeBits :: [Int] -> [Int] -> [Int]
aesEcbDecodeBits =  
    let decodeBits = aesEcbDecode `on` (fmap chr)
    in fmap ord .* decodeBits

solved7 = aesEcbDecode "YELLOW SUBMARINE" aesEcbText

-- 8
hasDuplicates blockSize xxs =
    let chunks = chunksOf blockSize xxs
    in length chunks /= length (nub chunks)

solved8 = 
    let bytes = aside hex2chars <$> ecbEncodedHex
    in filter (hasDuplicates 16 . snd) bytes

-- 9
padr l p xs =
    xs ++ take (l - length xs) (repeat p)
solved9 = padr 20 '\x04' "YELLOW SUBMARINE" == "YELLOW SUBMARINE\x04\x04\x04\x04"

-- 10
cbcDecode :: [Int] -> [Int] -> [Int]
cbcDecode key txt =
    let blockSize = length key
        iv = take blockSize $ repeat (ord '\x00')
        chunks = chunksOf blockSize $ txt
        decode1 cipher vec = 
            let dec = aesEcbDecodeBits key cipher
            in zipWith xor dec vec
        decoded = zipWith decode1 chunks (iv:chunks)
    in concat decoded

solved10 = 
    let d = cbcDecode `on` (fmap ord) 
    in fmap chr $ d "YELLOW SUBMARINE" cbcCipherText