fact :: Int -> Int
fact n
    | n < 0 = error "fact expects non-negative argument"
    | n == 0 = 1
    | otherwise = n * fact (n - 1)

main :: IO ()
main = print $ fact 5
