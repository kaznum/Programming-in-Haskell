--[f x | x <- xs, p x]
map f (filter p xs)
