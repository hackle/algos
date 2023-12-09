{-# LANGUAGE OverloadedStrings, TypeApplications #-}

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
import qualified Data.Text as T (pack)
import Data.Bits
import Data.List
import Data.Function
import Data.Tuple.Extra
import qualified Data.Map as M
import Data.Maybe (catMaybes, listToMaybe, mapMaybe, isJust)
-- import Data.Text.Metrics (hamming)
import GHC.Utils.Misc (fstOf3, sndOf3, thdOf3)
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error
import Control.Applicative
import Data.Composition
import System.Random
import Control.Monad
import Debug.Trace
import GHC.Data.Maybe (fromJust)
import Control.Monad.Extra (loop, pureIf)

toHex c2 = let [(n, _)] = readHex c2 in n

fromHexWith :: (Int -> a) -> String -> [a]
fromHexWith f xs = f . toHex <$> chunksOf 2 xs

hex2chars = fromHexWith chr
hex2ints = fromHexWith id

solve1 :: String -> String
solve1 = BT.unpack . encode . fromString . hex2chars

solved1 = solve1 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" == "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

fixedOr :: String -> String -> String
fixedOr xs ys =
    let xored = (zipWith xor `on` hex2ints) xs ys
    in concatMap (`showHex` "") xored

solved2 = fixedOr "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965" == "746865206b696420646f6e277420706c6179"

decodeXor keys x = fromHexWith (xor (ord x)) keys

toPrintable c = if isPrint c || isSpace c then Just c else Nothing

aside f = (,) <*> f

score :: [Char] -> Double
score xs = sum $ (\x -> M.findWithDefault 0 (toUpper x) freq) <$> xs

singleByteCipher :: [Char] -> String -> Maybe (Char, String, Double)
singleByteCipher keys xs =
    let scored = mapMaybe try1 keys
        sorted = sortBy (flip compare `on` thdOf3) scored
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
pad n p xs = replicate (n - length xs) p ++ xs

ord2hex = pad 2 '0' . flip showHex ""
char2hex = ord2hex . ord

str2hex :: String -> String
str2hex = concatMap char2hex

bytes2hex :: [Int] -> String
bytes2hex = concatMap ord2hex

-- repeatingKeyXor :: String -> String -> String
repeatingKeyXor xs k =
    bytes2hex $ zipWith (xor `on` ord) (cycle k) xs

solved5 = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f" == repeatingKeyXor "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal" "ICE"

int2bits ns = pad 8 '0' $ showIntAtBase 2 intToDigit ns ""
str2bits = concatMap (int2bits . ord)

hamming :: Eq a => [a] -> [a] -> Maybe Int
hamming xs ys =
    if ((==) `on` length) xs ys
        then Just $ length $ filter id $ zipWith (/=) xs ys
        else Nothing

hamming' :: String -> String -> Maybe Int
hamming' = hamming `on` str2bits

keyWeight keySize =
    let all4 = take 4 $ chunksOf keySize cypherText
        hms = catMaybes [ hamming' xs ys | xs <- all4, ys <- all4, xs /= ys ]
    in fromInteger (toInteger $ sum hms) / fromInteger (toInteger $ length hms) / fromInteger (toInteger keySize)

possibleKeySizes =
    let weights = aside keyWeight <$> [1..40]
    in take 5 $ sortOn snd weights

tryFindKeys keySizes =
    let chunks = transpose $ chunksOf keySizes cypherText
        candidates = singleByteXor . str2hex <$> chunks
        results = mapM (fmap fstOf3) candidates
    in results

breakRepeatingKeyXor keySizes =
    case mapMaybe tryFindKeys keySizes of
        [] -> Nothing
        (key:_) -> Just $ fromHexWith chr $ repeatingKeyXor cypherText key

ecbCodec codec key txt = BT.unpack $ codec cipher (BT.pack txt)
    where
        cipher :: AES128
        cipher = let (CryptoPassed c) = cipherInit (BT.pack key) in c

ecbDecode = ecbCodec ecbDecrypt
ecbEncode = ecbCodec ecbEncrypt

ecbDecodeBits = fmap ord .* ecbDecode `on` fmap chr
ecbEncodeBits = fmap ord .* ecbEncode `on` fmap chr

solved7 = ecbDecode "YELLOW SUBMARINE" aesEcbText

-- 8
hasDuplicates blockSize xxs =
    let chunks = chunksOf blockSize xxs
    in length chunks /= length (nub chunks)

solved8 =
    let bytes = aside hex2chars <$> ecbEncodedHex
    in filter (hasDuplicates 16 . snd) bytes

-- 9
padr l p xs =
    xs ++ replicate (l - length xs) p

modComp x y = negate x `mod` y

padBlocks blockSize xs =
    let compl = length xs `modComp` blockSize
    in xs ++ replicate compl eof

solved9 = padr 20 '\x04' "YELLOW SUBMARINE" == "YELLOW SUBMARINE\x04\x04\x04\x04"

unpadr p = dropWhileEnd (== p)

-- 10
eof = 4

cbcDecode :: [Int] -> [Int] -> [Int]
cbcDecode key txt = unpadr eof $ concat $ zipWith decode1 chunks (iv:chunks)
    where
        blockSize = length key
        iv = replicate blockSize 0
        chunks = chunksOf blockSize txt
        decode1 cipher vec =
            let dec = ecbDecodeBits key cipher
            in zipWith xor dec vec


cbcEncode :: [Int] -> [Int] -> [Int]
cbcEncode key txt = concat $ tail $ scanl encode1 iv chunks
    where
        blockSize = length key
        iv = replicate blockSize 0
        chunks = chunksOf blockSize txt
        encode1 prev chnk =
            let enc = zipWith xor (padr blockSize eof chnk) prev
            in ecbEncodeBits key enc

cbcDecodeStr = fmap chr .: cbcDecode `on` fmap ord
cbcEncodeStr = fmap chr .: cbcEncode `on` fmap ord

solved10 = cbcDecodeStr "YELLOW SUBMARINE" cbcCipherText

cbcIso = cbcDecodeStr "YELLOW SUBMARINE" (cbcEncodeStr "YELLOW SUBMARINE" solved10) == solved10

-- 11
chrGen = getStdRandom (randomR (0, 255))
algoGen = do
    idx <- getStdRandom (randomR (0, 1)) :: IO Int
    return $ case idx of
        0 -> traceShow "ECB" ecbEncodeBits
        1 -> traceShow "CBC" cbcEncode

randomBytes mn mx = do
    len <- getStdRandom $ randomR (mn, mx)
    replicateM len chrGen

encryptionOracle keyLength plainText = do
    key <- replicateM keyLength chrGen
    -- print key
    prefix <- randomBytes 5 10
    suffix <- randomBytes 5 10
    algo <- algoGen
    let fullText = prefix ++ plainText ++ suffix
        padded = padBlocks keyLength fullText
    return $ algo key padded

detectAlgo keyLength = do
    let plainText = replicate (keyLength * 3) 0
    cipher <- encryptionOracle keyLength plainText
    print $
        if hasDuplicates keyLength cipher
        then "ECB"
        else "CBC"

-- 12

hiddenKey keySize = replicateM keySize chrGen

ecb12 key plainText =
    let finalText = plainText ++ (ord <$> input12)
    in ecbEncodeBits key (padBlocks (length key) finalText) -- this also works with cbc

guessKeySize algo =
    let xss = tail $ scanl (flip (:)) [] $ repeat 0
        ciphers = algo <$> xss
        (l1, l2) = head $ dropWhile (uncurry (==)) $ zipWith ((,) `on` length) ciphers (tail ciphers)
    in abs $ l1 - l2

guessSalt algo known (filler:fillers) =
    maybe known (\c -> guessSalt algo (known++[c]) fillers) found
    where
        posFullBlocks = length filler + length known + 1
        good = take posFullBlocks $ algo filler
        mkGuess c = take posFullBlocks $ algo (filler ++ known ++ [c])
        found = find ((== good) . mkGuess) [0..255]

crackSalt plainText = do
    keySize <- (* 16) <$> getStdRandom (randomR (1, 1)) -- lib doesn't work with 32 :(
    key <- hiddenKey keySize
    let algo = ecb12 key
        guessedKeySize = guessKeySize (ecb12 key)
        fillers = concat $ repeat ((`replicate` 1) <$> reverse [0..(guessedKeySize - 1)])
        salt = guessSalt algo [] fillers
    print (chr <$> unpadr 4 salt)
