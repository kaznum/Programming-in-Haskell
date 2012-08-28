sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' (n+1) [] = []
take' (n+1) (x:xs) = x:take' n xs

last' :: [a] -> a
last' [x] = x
last' (_:xs) = last xs

dotProduct :: Num a => [a] -> [a] -> a
dotProduct xs ys = sum [x * y| (x, y) <- zip xs ys]