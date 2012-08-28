factors :: Int -> [Int]
factors n = [x|x <- [1..(n `div` 2)], n `mod` x == 0]
perfects :: Int -> [Int]
perfects n = [x| x <- [1..n],sum(factors x) == x]
