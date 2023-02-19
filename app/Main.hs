module Main where

import qualified Data.PQueue.Prio.Min as PQ
import Data.Function ((&))
import Data.List (find)
import Control.Monad

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

-- cycle detector
-- ghci> floyd []
-- Nothing
-- ghci> floyd [6..25]
-- Nothing
-- ghci> floyd $ concat $ repeat [6..25]
-- Just 6
-- ghci> floyd $ 1:2:3:4:5:(concat $ repeat [6..25])
-- Just 6
-- floyd :: Eq a => [a] -> Maybe a
floyd xs = do
    met <- race1 xs xs
    race2 xs met

race1 (x:xs) (_:y:ys) | x == y    = Just ys
                      | otherwise = race1 xs ys
race1 _ _ = Nothing

race2 (x:xs) (y:ys) | y == x    = Just y
                    | otherwise = race2 xs ys
race2 _ _ = Nothing

-- or zip

-- alternatives
floyd1 xs = race1 xs xs >>= race2 xs

floyd2 xs = race2 xs =<< race1 xs xs

floyd3 xs = xs & (race1 xs >=> race2 xs)


-- isSorted :: Ord a => [a] -> Bool
isSorted xs = compareLists xs (tail xs)

compareLists (x:xs) (y:ys) = x <= y && compareLists xs ys
compareLists _ [] = True
compareLists [] _ = False

-- ghci> digitSum 1
-- 1
-- ghci> digitSum 12
-- 3
-- ghci> digitSum 123
-- 6
-- ghci> digitSum 59
-- 5
-- ghci> digitSum 181
-- 1
-- ghci> digitSum 148148148
-- 3
digitSum n  | n <= 9 = n
            | otherwise = 
                let (q, r) = n `divMod` 10
                in  digitSum $ q + digitSum r
