{-# Language GeneralizedNewtypeDeriving #-}
module StringCompression where


compress :: String -> String
compress = formatTuples . compressToTuples 

compressToTuples :: String -> [(Char, Int)]
compressToTuples inp = reverse $ go inp []
    where
        go [] result = result
        go (inp1:inpRest) [] = go inpRest [(inp1, 1)]
        go (inp1:inpRest) result@((result1, resultCnt1):resultRest) = 
            if inp1 == result1
                then go inpRest ((result1, resultCnt1 + 1):resultRest)
                else go inpRest ((inp1, 1):result)

formatTuples :: [(Char, Int)] -> String
formatTuples xs = foldMap formatPair xs
    where 
        formatPair :: (Char, Int) -> String
        formatPair (chr, 1) = [chr]
        formatPair (chr, x) = chr:(show x)

-- compress "abcaaabbb" == "abca3b3"
-- compress "abcd" == "abcd"
-- compress "aaabaaaaccaaaaba" == "a3ba4c2a4ba"

newtype Prod a = Prod { unProd :: a } deriving (Show, Num, Eq, Ord)

instance (Num a) => Semigroup (Prod a) where
    (Prod a) <> (Prod b) = Prod (a * b)

instance (Num a) => Monoid (Prod a) where
    mempty = Prod 1

-- ghci> foldMap Prod ([1..5])
-- Prod {unProd = 120}
-- ghci> unProd $ foldMap Prod ([1..5])
-- 120