module Algo.DC where

shuffle :: [(str, str)] -> Maybe [(str, str, str)]
shuffle [] = Just []
shuffle [_] = Nothing
shuffle xs@(x:rest) = Just $ zipWith (\(x, _) (y, answer) -> (x, y, answer)) xs (rest ++ [x])