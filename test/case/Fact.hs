module Fact where

import UnsafeFact

fact :: Int -> Int
fact n
    | n < 0 = error $ "fact expects non-negative argument, got " ++ show n
    | otherwise = unsafeFact n
