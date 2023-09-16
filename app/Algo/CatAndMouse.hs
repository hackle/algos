{-# LANGUAGE GADTs, TypeFamilies, MultiParamTypeClasses, FlexibleContexts, TemplateHaskell #-}

module Algo.CatAndMouse where

import Data.Map as M
import Debug.Trace
import Control.Lens.Wrapped ( Wrapped (_Wrapped'), Rewrapped, Unwrapped )
import Control.Lens

data Result = CatWins | MouseWins | Draw deriving (Eq, Show)

opp CatWins = MouseWins
opp MouseWins = CatWins
opp r = r

data Player = Cat | Mouse deriving (Ord, Eq, Show)

newtype MousePos = M { _mPos :: Int } deriving (Ord, Eq, Show)
newtype CatPos = C { _cPos :: Int} deriving (Ord, Eq, Show)

makeLenses ''MousePos
makeLenses ''CatPos

type State = M.Map (MousePos, CatPos, Player) Result

tell (M 0) _ = MouseWins
tell _ (C 0) = error "Cat can't be here!"
tell (M m) (C c) = if m == c then CatWins else Draw

solve :: [[Int]] -> Result
solve graph =
    let finalState = bellmanFord moves state
    in finalState M.! (M 1, C 2, Mouse)
    where
        moves = M.fromList $ zip [0..] graph
        mx = length graph - 1
        state = M.fromList [ ((M m, C c, p), tell (M m) (C c)) | p <- [Mouse, Cat], m <- [0..mx], c <- [1..mx]]

bellmanFord :: M.Map Int [Int] -> State -> State
bellmanFord moves st0 =
    let st1 = M.foldrWithKey gather st0 st0
    in if st1 == st0 then st1 else bellmanFord moves st1
    where
        other Cat = Mouse
        other _ = Cat
        gather :: (MousePos, CatPos, Player) -> Result -> State -> State
        gather (M m, C c, p) _ st
            | st M.! (M m, C c, p) /= Draw = st
            | otherwise =
                let pos = if p == Mouse then m else c
                    desired = if p == Mouse then MouseWins else CatWins
                    valid pos1 = not (p == Cat && pos1 == 0)
                    setter = if p == Mouse then _1.mPos else _2.cPos
                    mStates = [ st M.! (setter .~ pos1 $ (M m, C c, other p)) | pos1 <- moves M.! pos, valid pos1 ]
                    res1 = aggr mStates desired
                in M.update (\_ -> Just res1) (M m, C c, p) st

aggr :: Foldable t => t Result -> Result -> Result
aggr probs desired
    | desired `elem` probs = desired
    | Draw `elem` probs = Draw
    | otherwise = opp desired

tests = all (\(e, t) -> solve t == e) [
    (MouseWins, [[1,3],[0],[3],[0,2]])
    , (Draw, [[2,5],[3],[0,4,5],[1,4,5],[2,3],[0,2,3]])
    , (MouseWins, [[2,3],[3,4],[0,4],[0,1],[1,2]])
    , (MouseWins, [[5,21,28],[6,8,9,13,23,24,30],[9,10,22,24],[24,30],[5,6,8,9,13,18,19,20,24],[0,4,9,10,11,12,22,27],[1,4,9,11,16,19,25,30],[8,9,13,19,25,26],[1,4,7,9,29],[1,2,4,5,6,7,8,13,18,19,24,26,28,29],[2,5,15,22,27,30],[5,6,12,24],[5,11,20,22,23],[1,4,7,9,29,30],[19,24,27],[10,16,19],[6,15,27],[20,22,24,29],[4,9,21],[4,6,7,9,14,15,20,26,28,30],[4,12,17,19,21],[0,18,20,27],[2,5,10,12,17],[1,12,26,30],[1,2,3,4,9,11,14,17,27,29],[6,7,26,27,29],[7,9,19,23,25],[5,10,14,16,21,24,25],[0,9,19,30],[8,9,13,17,24,25],[1,3,6,10,13,19,23,28]])
    , (CatWins, [[5,6],[3,4],[6],[1,4,5],[1,3,5],[0,3,4,6],[0,2,5]])
    , (MouseWins,[[3,4,6,7,9,15,16,18],[4,5,8,19],[3,4,6,9,17,18],[0,2,11,15],[0,1,10,6,2,12,14,16],[1,10,7,9,15,17,18],[0,10,4,7,9,2,11,12,13,14,15,17,19],[0,10,5,6,9,16,17],[1,9,14,15,16,19],[0,10,5,6,7,8,2,11,13,15,16,17,18],[4,5,6,7,9,18],[3,6,9,12,19],[4,6,11,15,17,19],[6,9,15,17,18,19],[4,6,8,15,19],[0,3,5,6,8,9,12,13,14,16,19],[0,4,7,8,9,15,17,18,19],[5,6,7,9,2,12,13,16],[0,10,5,9,2,13,16],[1,6,8,11,12,13,14,15,16]])
    ]