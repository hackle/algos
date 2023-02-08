module Main where

import qualified Data.PQueue.Prio.Min as PQ
import Data.Function ((&))

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- merge sort
tryInsert [] = id
tryInsert xs@(x:_) = PQ.insert x xs

sortN q | PQ.null q = []
        | otherwise =
            let (_, x:xs) = PQ.findMin q
                q1 = PQ.deleteMin q & tryInsert xs
            in x:sortN q1

mergeN :: Ord a => [[a]] -> [a]
mergeN = sortN . foldr tryInsert PQ.empty
