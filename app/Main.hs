module Main where

import qualified Data.PQueue.Prio.Min as PQ
import Data.Function ((&))

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- merge sort
tryInsert [] = id
tryInsert xs@(x:_) = PQ.insert x xs

mergeN q | PQ.null q = []
         | otherwise =
            let (_, x:xs) = PQ.findMin q
                q1 = PQ.deleteMin q & tryInsert xs
            in x:mergeN q1

sortN :: Ord a => [[a]] -> [a]
sortN = mergeN . foldr tryInsert PQ.empty
