data Tree = Leaf Int | Node Tree Tree

countOfLeaves :: Tree -> Int
countOfLeaves (Leaf _) = 1
countOfLeaves (Node a b) = countOfLeaves(a) + countOfLeaves(b)

balanced :: Tree -> Bool
balanced (Leaf _) = True
balanced (Node a b) =
  abs(countOfLeaves(a) - countOfLeaves(b)) <= 1 && balanced a && balanced b
