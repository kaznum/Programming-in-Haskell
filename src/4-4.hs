fst' (x,_) = x
snd' (_,y) = y


test' ('a':_) = True
test' _ = False

pred' :: Int -> Int
pred' 0 = 0
pred' (n + 1) = n

odds n = map(\x -> x * 2 + 1)[0..n-1]