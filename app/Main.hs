module Main where

import qualified Data.PQueue.Prio.Min as PQ
import Data.Function ((&))

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- merge sort
tryInsert [] = id
tryInsert xs@(x:_) = PQ.insert x xs

mergeN :: Ord a => [[a]] -> [a]
mergeN xss = go initQ
    where
        initQ = foldr tryInsert PQ.empty xss
        go q =
            if PQ.null q then [] else
            let (_, x:xs) = PQ.findMin q
                q1 = PQ.deleteMin q & tryInsert xs & PQ.filter (not.null)
            in x:go q1