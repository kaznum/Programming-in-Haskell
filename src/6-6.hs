product' :: Num a => [a] -> a
product' = foldr (*) 1


drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' (n+1) [] = []
drop' (n+1) (_:xs) = drop n xs

init' :: [a] -> [a]
init' [_] = []
init' (x:xs) = x:init xs
