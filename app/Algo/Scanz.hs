module Algo.Scanz where

scanlz :: (a -> b -> a) -> a -> [b] -> [a]
scanlz f z0 xs = let zs = z0 : zipWith f zs xs in zs