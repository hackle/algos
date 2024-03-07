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
import Data.List.Split hiding (sepBy, chunksOf)
import qualified Data.Text as T (pack)
import Data.Bits
import Data.List
import Data.Function
import Data.Tuple.Extra
import qualified Data.Map as M
import Data.Maybe (catMaybes, listToMaybe, mapMaybe, isJust, fromMaybe)
-- import Data.Text.Metrics (hamming)
import GHC.Utils.Misc (fstOf3, sndOf3, thdOf3, applyWhen)
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
import Text.Parsec hiding (eof, State)
import Text.Parsec.Char hiding (eof)
import Data.List.Extra (dropSuffix, chunksOf)
import Data.Binary.Builder
import Data.ByteString.Builder
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Int
import Data.Word
import System.Random
import Control.Monad.State (get, put, runState, State, evalState)
import GHC.Utils.Misc (nTimes)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Time.Extra (sleep)

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

padBlocks :: Int -> a -> [a] -> [a]
padBlocks blockSize filler xs =
    let compl = length xs `modComp` blockSize
    in xs ++ replicate compl filler

solved9 = padr 20 '\x04' "YELLOW SUBMARINE" == "YELLOW SUBMARINE\x04\x04\x04\x04"

unpadr p = dropWhileEnd (== p)

-- 10
eofMarker = 4
eofMarkerChar = chr eofMarker

cbcDecode :: [Int] -> [Int] -> [Int] -> [Int]
cbcDecode iv key txt = concat $ zipWith decode1 chunks (iv:chunks)
    where
        blockSize = length key
        chunks = chunksOf blockSize txt
        decode1 cipher vec =
            let dec = ecbDecodeBits key cipher
            in zipWith xor dec vec


cbcEncode :: [Int] -> [Int] -> [Int] -> [Int]
cbcEncode iv key txt = concat $ tail $ scanl encode1 iv chunks
    where
        blockSize = length key
        chunks = chunksOf blockSize txt
        encode1 prev chnk =
            let enc = zipWith xor (padr blockSize eofMarker chnk) prev
            in ecbEncodeBits key enc

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 f g a0 a1 a2 =  f (g a0) (g a1) (g a2)

cbcDecodeStr = fmap chr .:. cbcDecode `on3` fmap ord
cbcEncodeStr = fmap chr .:. cbcEncode `on3` fmap ord

myIV = "abcdABCDefghEFGH"
myIVBits = ord <$> myIV

solved10 = cbcDecodeStr myIV "YELLOW SUBMARINE" cbcCipherText

cbcIso = cbcDecodeStr myIV "YELLOW SUBMARINE" (cbcEncodeStr myIV "YELLOW SUBMARINE" solved10) == solved10

-- 11
chrGen = getStdRandom (randomR (0, 255))
algoGen = do
    idx <- getStdRandom (randomR (0, 1)) :: IO Int
    return $ case idx of
        0 -> traceShow "ECB" ecbEncodeBits
        1 -> traceShow "CBC" $ cbcEncode myIVBits

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
        padded = padBlocks keyLength eofMarker fullText
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
    in ecbEncodeBits key (padBlocks (length key) eofMarker finalText) -- this also works with cbc

guessKeySize algo =
    let xss = tail $ scanl (flip (:)) [] $ repeat 0
        ciphers = algo <$> xss
        (l1, l2) = head $ dropWhile (uncurry (==)) $ zipWith ((,) `on` length) ciphers (tail ciphers)
    in abs $ l1 - l2

guessSalt algo known filler =
    maybe known (\c -> known++[c]) found
    where
        found = find test [0..255]
        test c = run filler == run (filler ++ known ++ [c])
        run = take (length filler + length known + 1) . algo

crack12 plainText = do
    keySize <- (* 16) <$> getStdRandom (randomR (1, 1)) -- lib doesn't work with 32 :(
    key <- hiddenKey keySize
    let algo = ecb12 key
        guessedKeySize = guessKeySize (ecb12 key)
        totalAttepmpts = traceShowId $ length $ algo []
        fillers = take totalAttepmpts $ cycle ((`replicate` 1) <$> reverse [0..(guessedKeySize - 1)])
        result1 = foldl (guessSalt algo) [] fillers
        -- alternatively, much fancier
        result2 = let known = zipWith (guessSalt algo) ([]:known) fillers in last known

    print (chr <$> unpadr 4 result1)
    print (chr <$> unpadr 4 result2)

-- 13
kvParser :: Parsec String String (M.Map String String)
kvParser = do
    kvps <- pair `sepBy1` char '&'
    return $ M.fromList kvps
    where
        pair = do
            k <- many1 letter
            char '='
            v <- many1 letter
            return (k, v)

parseKvp = runParser kvParser "" "kvp"

encodeKvps m = intercalate "&" $ fmap (\(a, b) -> a ++ "=" ++ b) m

profileFor email = encodeKvps [("email", sanitise email), ("uid", "10"), ("role", "user")]
    where
        sanitise = filter (not . (`elem` ("&="::String)))

isolateUser blockSize = replicate padBlock0 '\x04' ++ padr blockSize '\x04' "admin" ++ replicate padBlock2 '\x04'
    where
        padBlock0 = blockSize - length ("email="::String)
        padBlock2 = padSize - padBlock0
        bareBone = profileFor ""
        formatSize = length ("email=&"::String)
        padSize = blockSize - length (dropSuffix "user" bareBone) `mod` blockSize

hackIt13 secret =
    let [b0, b1, b2, b3] = chunksOf 16 $ ecbEncode (chr <$> secret) $ padBlocks 16 eofMarkerChar $ profileFor $ isolateUser 16
    in ecbDecode (chr <$> secret) $ concat [b0, b2, b1]

ecbCutPaste = do
    secret <- hiddenKey 16
    let hacked = hackIt13 secret
    putStrLn hacked

-- 14

ecb14 prefix key plainText =
    let finalText = prefix ++ plainText ++ (ord <$> input12)
    in ecbEncodeBits key (padBlocks (length key) eofMarker finalText) -- this also works with cbc

crack14 plainText = do
    let keySize = 16 -- lib doesn't work with 32 :(
    prefixSize <- getStdRandom (randomR (1, 16))
    key <- hiddenKey keySize
    prefix <- hiddenKey prefixSize
    let algo = ecb14 prefix key
        guessedKeySize = guessKeySize algo
        padSize = head $ filter (\x -> hasDuplicates guessedKeySize $ algo (replicate (x + guessedKeySize * 2) eofMarker)) [0..guessedKeySize]
        padding = replicate padSize eofMarker
        totalAttempts = length (algo padding) - guessedKeySize
        fillers = take totalAttempts $ cycle ((`replicate` 1) <$> reverse [0..(guessedKeySize - 1)])
        padAlgo xs = drop guessedKeySize $ algo (padding ++ xs)
        result1 = foldl (guessSalt padAlgo) [] fillers

    print (chr <$> unpadr 4 result1)

-- 16
cbc16 key plainText = cbcEncodeStr key $ prefix ++ sanitise plainText ++ suffix
    where
        masks = M.fromList [(';', '9'), ('=', '8')]
        sanitise = map (\x -> fromMaybe x (x `M.lookup` masks))
        prefix = "comment1=cooking%20MCs;userdata="
        suffix = ";comment2=%20like%20a%20pound%20of%20bacon"



flipBits c = chr $ 255 `xor` ord c
flippedChars = M.fromList [ aside flipBits x | x <- [';', '='] ]

safeInjection = (\x -> fromMaybe x (x `M.lookup` flippedChars)) <$> ("nice;admin=true;" :: String)

crack16 plain =
    let key = "YELLOW SUBMARINE"
        cipher = cbc16 myIV key plain
        (b0:b1:bs) = chunksOf 16 cipher
        targetIndice = [4,10,15]
        danger = zipWith (\i x -> applyWhen (i `elem` targetIndice) flipBits x) [0..] b1
        plain1 = cbcDecodeStr key (concat $ b0:danger:bs)
    in plain1

-- 17
pkcs7 xs blockSize = xs ++ padding
    where padding = let m = blockSize - length xs `mod` blockSize in replicate m m

validPkcs7 [] = True
validPkcs7 xs =
    let (x:xs') = reverse xs
    in ord x == 1 + length (takeWhile (== x) xs')

check17 iv key ys =
    let x = cbcDecodeStr iv key ys
    in pureIf (validPkcs7 x) x

zipRightWith f = reverse .: (zipWith f `on` reverse)
xorChr = xor `on` ord

guessByPadding check iv cipher =
    foldr go [] [0..15]
    where
        go idx known =
            let prev = take idx iv
                paddingLen = 16 - idx
                -- reverse-engineers padding into IV, so the pkcs validation will pass
                padding = zipRightWith (\a b -> chr $ a `xorChr` b `xor` paddingLen) known iv
                mkIV x = prev ++ [chr x] ++ padding
                attackIVs = aside mkIV <$> [0..255]
                match = find (const True) [ x | (x, iv) <- attackIVs, isJust (check1 iv) ]
            in
                case match of
                    Nothing -> known
                    Just x -> let n = chr $ x `xor` paddingLen `xor` ord (iv !! idx) in n:known
        check1 iv = check iv cipher

crack17 = do
    key <- hiddenKey 16
    pIdx <- getStdRandom (randomR (0, 9))
    let keyChars = chr <$> key
        plain = padBlocks 16 '\x17' (input17 !! pIdx)   -- 17 because pkcs can go up to 16
        ciphers = chunksOf 16 $ cbcEncodeStr myIV keyChars plain
        guess1 = guessByPadding (`check17` keyChars)
    putStr $ concat $ zipWith guess1 (myIV:ciphers) ciphers

-- 18

toLE :: Int -> [Char]
toLE n = unpack $ toLazyByteString (int64LE $ fromInteger $ toInteger n)

keyStream :: [Char] -> Int -> [Char]
keyStream key nonce = concatMap ((ecbEncode key . (toLE nonce ++)) . toLE) [0..]

ctrStreamEncode :: [Char] -> Int -> [Char] -> [Int]
ctrStreamEncode key nonce plain = zipWith xor (ord <$> plain) (ord <$> keyStream key nonce)

crack18 = chr <$> ctrStreamEncode "YELLOW SUBMARINE" 0 input18

-- 21
data MTGen = MTGen [Word32] Int


twists = [
    \y -> y `xor` (y `shiftR` 11)
    ,\y -> y `xor` (y `shiftL` 7 .&. 0x9d2c5680)
    ,\y -> y `xor` (y `shiftL` 15 .&. 0xefc60000)
    ,\y -> y `xor` (y `shiftR` 18)
    ]

initialize :: Word32 -> MTGen
initialize seed = MTGen (iterate twist seed) 0
  where 
    twist x = f * (x `xor` (x `shiftR` 30)) + 1
    f = 1812433253

extract :: State MTGen Word32
extract = do
    (MTGen list i) <- get
    let list' = if i + 1 == 624 then generate list else list
    put $ MTGen list' (i + 1)
    return $ temper (list !! i)
  where
    temper y = foldl (&) y twists
    generate (x:xs) =
        x .&. upperMask + (xs !! (n - m)) `xor` (if testBit x (w - 1) then a else 0) : xs
    upperMask = shiftL (complement 0) r
    a = 0x9908b0df
    (w, n, m, r) = (32, 624, 397, 31)

extractN s n = evalState (replicateM n extract) (initialize s)

-- 22
waitRand = getStdRandom (randomR (1, 10))
crackSeed = do
    seed <- getPOSIXTime
    n <- waitRand
    putStrLn $ "Sleeping for seconds " ++ show n
    sleep n
    let secret = extract1 (floor seed)
    nowP <- getPOSIXTime
    let found = find (\s1 -> secret == extract1 (floor nowP - s1)) [1..100]
    print $ (floor nowP -) <$> found
    where extract1 s = last $ extractN s 1

maxN :: Double
maxN = 2 ^ 32 - 1

isUniform s n = do
    let topN = extractN s n
        normalised = floor . ((fromInteger . toInteger $ n) *). (/ maxN) . fromInteger . toInteger <$> topN
    print normalised

-- not the best in distribution
distribution :: Int -> IO ()
distribution n = do
    let mw = maxBound :: Word32
        range = [0, mw `div` (fromInteger . toInteger $ n) .. mw ]
    mapM_ (`isUniform` n) range

-- ghci> distribution 5
-- [0,0,1,3,0]
-- [0,1,3,0,4]
-- [0,2,3,2,3]
-- [2,2,1,1,0]
-- [2,2,1,3,3]
-- [2,4,0,0,3]