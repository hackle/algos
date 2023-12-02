{-# LANGUAGE OverloadedStrings #-}

module Algo.HexToBase64 where

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

toHex c2 = let [(n, _)] = readHex c2 in n

fromHexWith :: (Int -> a) -> String -> [a]
fromHexWith f xs = (f . toHex) <$> chunksOf 2 xs

solve1 :: String -> String
solve1 = BT.unpack . encode . fromString . (fromHexWith chr)

solved1 = solve1 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" == "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

fixedOr :: String -> String -> String
fixedOr xs ys = concatMap (flip showHex "") $ zipWith xor (fromHexWith id xs) (fromHexWith id ys)

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

(Right cypherText) = file

file = fmap BT.unpack $ decode $ fromString $ concat $ [
    "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"
    ]

freq = M.fromList [
    (' ', 20)
    ,('E', 12.02)
    ,('T', 9.10)
    ,('A', 8.12)
    ,('O', 7.68)
    ,('I', 7.31)
    ,('N', 6.95)
    ,('S', 6.28)
    ,('R', 6.02)
    ,('H', 5.92)
    ,('D', 4.32)
    ,('L', 3.98)
    ,('U', 2.88)
    ,('C', 2.71)
    ,('M', 2.61)
    ,('F', 2.30)
    ,('Y', 2.11)
    ,('W', 2.09)
    ,('G', 2.03)
    ,('P', 1.82)
    ,('B', 1.49)
    ,('V', 1.11)
    ,('K', 0.69)
    ,('X', 0.17)
    ,('Q', 0.11)
    ,('J', 0.10)
    ,('Z', 0.07)
    ]