factorial :: Int -> Int
factorial 0 = 1
factorial (n+1) = (n+1) * factorial n

product' :: Num a => [a] -> a
product' [] = 1
product' (n:ns) = n * product ns

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci (n + 2) = fibonacci (n + 1) + fibonacci n

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
                 where
                   smaller = [a|a <- xs, a <= x]
                   larger = [a|a <- xs, a > x]
