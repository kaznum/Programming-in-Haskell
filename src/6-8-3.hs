and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' (n+1) x = x:replicate' n x

(!!!) :: [a] -> Int -> a
(x:_) !!! 0 = x
(_:xs) !!! (n+1) = xs !!! n

elem' :: Eq a => a -> [a] -> Bool
elem' m [] = False
elem' m (n:ns)| m == n = True
              | otherwise = elem' m ns