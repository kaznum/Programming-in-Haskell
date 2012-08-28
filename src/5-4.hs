import Char
--lowers :: String -> Int
lowers :: [Char] -> Int
lowers cs = length[1|c <- cs,isLower c]

count :: Char -> String -> Int
count x xs = length[1|x' <- xs, x' == x]
