module Algo.RepMin where


data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)

repMin :: Ord a => (Tree a, a) -> (Tree a, a)
repMin (Leaf a, m) = (Leaf m, a)
repMin (Node l r, m) = (Node l' r', ml `min` mr)
    where
        (l', ml) = repMin (l, m)
        (r', mr) = repMin (r, m)

trace :: ((a, c) -> (b, c)) -> a -> b
trace f a = b
    where
        (b, c) = f (a, c)


-- swap1 :: Show a, Read b => (a, String) -> (b, String)
swap1 (a, c) = ((c, c), [a])