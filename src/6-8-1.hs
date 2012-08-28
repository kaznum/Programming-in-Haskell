ex :: Int -> Int -> Int
ex x 0 = 1
ex x (n + 1) = x * (ex x n)
