data Tree = Leaf Int | Node Tree Tree
balance :: [Int] -> Tree

halves xs = splitAt (length xs `div` 2) xs

balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs)
             where (ys, zs) = halves xs
