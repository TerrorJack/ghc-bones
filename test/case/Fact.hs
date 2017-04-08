module Fact where

fact :: Int -> Int
fact n
    | n < 0 = error $ "fact expects non-negative argument, got " ++ show n
    | n == 0 = 1
    | otherwise = n * fact (n - 1)
