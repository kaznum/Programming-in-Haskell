merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x:(merge xs (y:ys))
                | otherwise = y:(merge (x:xs) ys)


halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve xs = splitAt (length(xs) `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort as) (msort bs)
             where (as,bs) = halve xs
