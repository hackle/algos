module Algo.Cycle where

import Algo.State ( State(..), getState, modifyState )
import Debug.Trace ()
import Data.Tuple (swap)
import qualified Data.Map.Strict as M

type Max = Int
type Count = Int

toMapIndexed :: [Int] -> M.Map Int Int
toMapIndexed xs = M.fromList $ zip [0..] xs

sanitise :: M.Map Int Int -> M.Map Int Int
sanitise mp = 
    let reversed = M.fromList $ fmap swap (M.toList mp)
        rest = M.filterWithKey (\k _ -> k `M.member` reversed) mp
    in if rest == mp 
        then mp 
        else sanitise rest

walk1 :: Int -> Count -> State (M.Map Int Int) Max
walk1 k cnt = do
    mp <- getState
    case k `M.lookup` mp of
        Nothing -> return cnt   -- must have finished the loop; input gurantees to have loops
        Just k1 -> do
            modifyState (M.delete k)
            walk1 k1 (cnt + 1)

walkOn :: State (M.Map Int Int) Max
walkOn = go 0
    where 
        go mx = do
            mp <- getState
            if M.null mp || M.size mp < mx  -- optimisation: less than max left
                then return mx
                else do
                    let (k, a) = 0 `M.elemAt` mp
                    mx1 <- walk1 k 1
                    go (max mx mx1)

longestCycle :: [Int] -> Max
longestCycle xs = 
    let pairs = sanitise (toMapIndexed xs)
        (maxLength, _) = let (State f) = walkOn in f pairs
    in maxLength - 1

-- longestCycle [2,-1,3,1] == -1
-- longestCycle [3,3,4,2,3] == 3
-- longestCycle [1,2,0,4,5,6,3,8,9,7] == 4;