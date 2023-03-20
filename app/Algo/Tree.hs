{-# Language DeriveFunctor #-}
module Algo.Tree where

import Control.Monad.State
import Data.List.Split

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show, Functor)

toLeaf Nothing = Empty
toLeaf (Just a) = Node a Empty Empty

toNode Nothing Empty Empty = Empty
toNode (Just x) Empty Empty = Node x Empty Empty
toNode (Just x) l r = Node x l r

parseTree :: [Maybe Int] -> Tree Int
parseTree xs = let [t] = go 0 xs in t
    where
        go n [] = []
        go n xs = 
            let (currents, nexts) = splitAt (2 ^ n) xs
                children = go (n + 1) nexts
            in buildTree currents children

buildTree :: [Maybe Int] -> [Tree Int] -> [Tree Int]
buildTree xs [] = toLeaf <$> xs
buildTree (x:xs) [t] = toNode x t Empty : buildTree xs []
buildTree (x:xs) (t1:t2:ts) = toNode x t1 t2 : buildTree xs ts

toNullableInts :: String -> [Maybe Int]
toNullableInts str = fmap parse1 raw
    where 
        raw = splitOn "," $ tail $ init str
        parse1 "null" = Nothing
        parse1 s = Just $ read s

data LCA a = NotFound | Only a | Found a deriving (Eq, Show)

lca :: Eq a => a -> a -> Tree a -> LCA a
lca _ _ Empty = NotFound
lca x y (Node v t1 t2) = foldl (mergeLca v) NotFound [comp1 [x, y] v, lca x y t1, lca x y t2]

comp1 xs a = if a `elem` xs then Only a else NotFound

mergeLca v l r =
    case (l, r) of
        (Found f1, _) -> Found f1
        (_, Found f2) -> Found f2
        (NotFound, NotFound) -> NotFound
        (NotFound, Only n2) -> Only n2
        (Only n1, NotFound) -> Only n1
        (Only n1, Only n2) -> Found v  -- n1 being the top one

testInput = "[3,5,1,6,2,0,8,null,null,7,4]"



