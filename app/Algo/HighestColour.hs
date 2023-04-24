module Algo.HighestColour where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Debug.Trace

-- highestColour :: [Char] -> [[Int]] -> Int
highestColour colours adjancent =
    let mColours = M.fromList $ zip colours [0..]
        outs = M.fromListWith (++) $ fmap (\[x, y] -> (x, [y])) adjancent
        ins = M.fromListWith (++) $ fmap (\[x, y] -> (y, [x])) adjancent
        loose = M.filterWithKey (\k _ -> not $ M.member k ins) outs
    in trace (show ins ++ show outs ++ show loose) $ solve mColours ins outs loose


delete1 :: M.Map Int [Int] -> Int -> [Int] -> M.Map Int [Int]
delete1 ins k = L.foldl (flip (M.update (\v -> let v' = L.delete k v in if L.null v' then Nothing else Just v'))) ins


solve :: M.Map Char Int -> M.Map Int [Int] -> M.Map Int [Int] -> M.Map Int [Int] -> M.Map Int [Int]
solve mColours ins outs loose =
    let loose' = M.fromList $ concatMap (\(k, xs) -> (\x -> (x, M.findWithDefault [] x outs)) <$> xs) (M.toList loose)
        ins' = M.foldlWithKey delete1 ins loose
    in trace (show ins' ++ show loose') $
        if ins' == ins
            then ins'
            else solve mColours ins' outs loose'



-- highestColour "abaca" [[0,1],[0,2],[2,3],[3,4]]