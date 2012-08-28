primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (x:xs) = x:sieve [n|n <- xs, n `mod` x /= 0]

