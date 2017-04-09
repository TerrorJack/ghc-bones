module UnsafeFact where

unsafeFact :: Int -> Int
unsafeFact 0 = 1
unsafeFact n = n * unsafeFact (n - 1)
