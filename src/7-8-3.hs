import Char
-- map f [1,2,3]
-- 1:2:3:[] -> f 1:f 2:f 3:[]
map' f = foldr (\x y -> f x:y) []

--filter p [1,2,3,4]
-- 1:2:4:[]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []