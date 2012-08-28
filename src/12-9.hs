fibs :: [Integer]
fibs = 0:1:[x + y|(x,y) <- zip fibs (tail fibs)]

fib :: Int -> Integer
--fib n = head(drop n (take (n + 1) fibs))
fib n = fibs !! n


