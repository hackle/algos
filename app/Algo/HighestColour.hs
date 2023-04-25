module Algo.HighestColour where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import Debug.Trace
import qualified Data.Foldable as F

-- highestColour :: [Char] -> [[Int]] -> Int
highestColour colours adjancent =
    let coloursIndexed = M.fromList $ zip [0..] colours
        outs = M.fromListWith (++) $ fmap (\[x, y] -> (x, [y])) adjancent
        ins = M.fromListWith S.union $ fmap (\[x, y] -> (y, S.singleton x)) adjancent
        loose = M.filterWithKey (\k _ -> not $ M.member k ins) outs
        coloured = M.mapKeys (colours L.!!) loose
        paths = M.mapWithKey (\k _ -> [[k]]) loose
        maxes = M.mapWithKey (\k _ -> M.fromList [(coloursIndexed M.! k, 1)]) loose
        finals = solve coloursIndexed ins outs loose maxes (-1)
    in --trace (show [ show ins, show outs, show loose, show finals ]) $ 
        finals

-- trace _ n = n


countMax :: String -> [Int] -> Int
countMax colours indice = F.maximum $ M.fromListWith (+) $ [ (colours L.!! x, 1) | x <- indice ]

delete1 :: M.Map Int (S.Set Int) -> Int -> [Int] -> M.Map Int (S.Set Int)
delete1 ins k = L.foldl (flip (M.update (\v -> let v' = S.delete k v in if S.null v' then Nothing else Just v'))) ins


extract :: M.Map Char Int -> Int
extract = F.maximum

solve ::
    M.Map Int Char -- colours
    -> M.Map Int (S.Set Int) -- ins
    -> M.Map Int [Int] -- outs
    -> M.Map Int [Int] -- loose
    -> M.Map Int (M.Map Char Int) -- maxes
    -- -> Int -- done
    -> Int   -- done
    -> Int
solve colours ins outs loose maxes done =
    let (loose', ins', maxes', done') = M.foldlWithKey follow1 (M.empty, ins, maxes, done) loose
        -- ins' = M.foldlWithKey delete1 ins loose
    in --trace (show ["Ins: " ++ show ins' ++ " Loose: " ++ show loose' ++ " Maxes: " ++ show maxes' ]) $
        if M.null ins'
            then done'
            else
                if ins' == ins -- stuck
                    then trace "It's stuck, giving up" (-1)
                    else trace ("Currently ins " ++ show ins' ++ " Loose " ++ show loose' ++ " Maxes: " ++ show maxes') $ 
                            solve colours ins' outs loose' maxes' done'
    where
        -- increColour k = M.insertWith (1 +) (colours L.!! k) 1
        update1 ::
            Int
            -> M.Map Int (S.Set Int)
            -> (M.Map Int [Int], M.Map Int (M.Map Char Int), Int)
            -> Int -> (M.Map Int [Int], M.Map Int (M.Map Char Int), Int)
        update1 k ins2 (ls, ms, dn) x =
            let mx2k = ms M.! k
                ms1 = M.insertWith (M.unionWith max) x (M.insertWith (+) (colours M.! x) 1 mx2k) ms
            in if x `M.member` ins2 -- not loose, still has incoming
                        then (ls, ms1, dn)
                        else if x `M.member` outs   -- loose, still has outs
                                then (M.insert x (outs M.! x) ls, ms1, dn)
                                else (ls, M.delete x ms1, L.foldl' max dn (ms1 M.! x)) -- no outs, done
        follow1 ::
            (M.Map Int [Int], M.Map Int (S.Set Int), M.Map Int (M.Map Char Int), Int)
            -> Int
            -> [Int]
            -> (M.Map Int [Int], M.Map Int (S.Set Int), M.Map Int (M.Map Char Int), Int)
        follow1 (loose1, ins1, maxes1, done1) k xs =
            let ins2 = delete1 ins1 k xs
                        -- ps1 = L.foldl (\st p -> M.insertWith (++) x [x:p] st) ps pathsToK
                (loose2, maxes2, done2) = L.foldl (update1 k ins2) (loose1, maxes1, done1) xs
            in (loose2, ins2, M.delete k maxes2, done2)




-- highestColour "abaca" [[0,1],[0,2],[2,3],[3,4]] -- 3 
-- highestColour "bbbhb" [[0,2],[3,0],[1,3],[4,1]] -- 4
-- highestColour "nnllnzznn" [[0,1],[1,2],[2,3],[2,4],[3,5],[4,6],[5,6],[6,7],[7,8]] -- 5
-- highestColour "eeyyeeyeye" [[0,1],[1,2],[2,3],[3,4],[4,5],[4,6],[5,7],[6,8],[8,9]] -- 5
-- highestColour "nnnnnnnnnn" [[0,1],[1,2],[2,3],[3,4],[4,5],[5,6],[5,7],[3,7],[3,8],[5,9],[6,9]] == 8
-- highestColour "bwsswpwbpwpsbswbwswbwbbbwpwsbsssw" [[0,1],[1,2],[2,3],[3,4],[4,5],[3,5],[2,6],[5,7],[6,8],[7,8],[4,9],[8,9],[7,9],[9,10],[8,10],[5,10],[9,11],[10,12],[11,12],[9,12],[12,13],[10,13],[11,13],[8,14],[13,14],[12,14],[14,15],[13,15],[11,15],[10,16],[13,17],[10,17],[12,17],[15,17],[8,18],[17,18],[10,18],[14,19],[10,19],[18,19],[14,20],[18,20],[16,20],[19,20],[20,21],[18,21],[16,22],[21,22],[20,22],[14,23],[22,23],[21,23],[20,23],[18,24],[16,24],[22,24],[21,24],[18,25],[22,25],[21,25],[24,25],[20,25],[25,26],[25,27],[25,28],[27,29],[22,29],[28,29],[23,29],[28,30],[24,31],[28,31],[27,31],[29,32],[28,32],[30,32]] == 8
-- highestColour "qqxfhffrqxqbhhrfrsfxbfxhxxhsfbhbfqqfrsqsqhbrmhmsqxrhfxhffssmrfxhr" [[0,1],[1,2],[2,3],[0,3],[3,4],[4,5],[5,6],[6,7],[4,7],[7,8],[6,9],[7,9],[8,9],[5,9],[8,10],[7,10],[10,11],[9,11],[8,11],[11,12],[5,12],[11,13],[12,13],[13,14],[12,14],[8,14],[10,14],[14,15],[13,15],[12,15],[15,16],[12,16],[8,16],[16,17],[15,18],[18,19],[17,19],[19,20],[12,20],[17,20],[20,21],[18,21],[19,22],[21,22],[22,23],[21,23],[22,24],[23,25],[24,25],[22,25],[25,26],[26,27],[20,27],[25,28],[13,28],[26,28],[25,29],[27,30],[30,31],[13,31],[28,31],[31,32],[26,32],[21,32],[27,32],[30,33],[32,33],[31,33],[26,33],[31,34],[25,34],[23,34],[5,35],[32,35],[30,36],[20,36],[29,36],[35,36],[35,37],[34,37],[36,37],[32,37],[27,38],[19,39],[28,39],[5,39],[38,40],[39,40],[22,41],[35,41],[38,41],[40,41],[24,42],[40,42],[30,43],[40,43],[41,43],[39,44],[22,45],[41,45],[33,45],[43,45],[42,46],[43,46],[44,46],[44,47],[30,47],[43,48],[47,48],[48,49],[48,50],[49,50],[45,51],[34,51],[37,51],[45,52],[49,53],[36,53],[52,54],[46,54],[53,55],[52,56],[55,56],[51,57],[56,57],[50,57],[53,58],[35,58],[43,59],[47,59],[54,59],[45,60],[57,60],[47,60],[58,61],[35,61],[61,62],[52,63],[48,63],[47,63],[56,64],[61,64],[52,64]] == 11
-- highestColour "dnlnlhlchdhncccycnhnnhhynyhdhcdcyyddchclcdnyncnhclhnnlchndcyhcyydnhhyccndlhycylhnhyldlhyyhyhlnhh" [[0,1],[1,2],[1,3],[2,3],[2,4],[3,4],[4,5],[3,5],[5,6],[4,6],[5,7],[6,7],[6,8],[7,8],[8,9],[7,9],[8,10],[9,10],[5,11],[10,11],[11,12],[7,12],[12,13],[11,13],[13,14],[6,15],[10,15],[14,15],[13,15],[14,16],[15,16],[7,16],[11,17],[14,17],[10,17],[16,17],[16,18],[14,18],[14,19],[18,19],[19,20],[14,20],[20,21],[21,22],[18,23],[17,23],[22,23],[20,23],[14,23],[21,24],[23,24],[22,24],[24,25],[22,25],[21,25],[22,26],[23,27],[26,27],[11,27],[19,27],[18,28],[27,28],[26,29],[28,29],[25,29],[24,30],[29,30],[29,31],[30,32],[31,33],[30,33],[32,33],[20,34],[33,35],[34,35],[22,35],[29,36],[34,36],[31,36],[35,37],[36,37],[33,37],[32,38],[25,38],[29,39],[30,39],[38,40],[22,40],[39,40],[40,41],[34,41],[35,41],[41,42],[40,42],[25,43],[40,43],[42,43],[20,43],[34,44],[43,44],[42,44],[38,45],[24,45],[39,45],[45,46],[42,46],[34,46],[42,47],[41,47],[46,47],[44,47],[36,48],[45,48],[43,49],[47,49],[42,49],[41,50],[47,51],[26,51],[42,51],[46,51],[51,52],[47,52],[21,53],[50,54],[44,54],[48,54],[49,55],[53,55],[43,56],[36,56],[55,57],[57,58],[55,59],[49,59],[36,59],[43,59],[59,60],[31,60],[55,60],[58,60],[55,61],[53,62],[59,62],[61,63],[57,64],[58,64],[42,64],[64,65],[63,65],[63,66],[66,67],[55,67],[60,67],[50,68],[52,69],[58,69],[64,69],[63,69],[57,69],[69,70],[65,70],[26,71],[70,71],[62,71],[67,71],[32,71],[39,72],[70,72],[68,73],[67,73],[59,73],[64,74],[68,74],[42,74],[69,74],[70,74],[51,75],[68,75],[53,75],[70,75],[73,75],[71,76],[72,76],[71,77],[70,78],[74,78],[74,79],[73,79],[66,79],[78,79],[76,79],[65,80],[78,80],[61,80],[79,80],[78,81],[70,81],[81,82],[71,82],[78,82],[75,82],[74,83],[82,83],[45,83],[77,84],[82,85],[71,85],[85,86],[67,86],[83,86],[36,87],[74,87],[80,88],[70,88],[78,88],[75,88],[15,88],[87,89],[88,89],[82,89],[79,89],[64,90],[89,90],[78,90],[87,90],[89,91],[47,92],[86,92],[87,93],[93,94],[71,94],[32,95],[94,95]] == 15