data Op = Add|Sub|Mul|Div|Exp
        deriving (Show)

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0
valid Exp x y = x /= 1 && y /= 1

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr
          deriving (Show)

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r
eval :: Expr -> [Int]
--eval (Val n) = [n | n > 0]
eval (Val n) = [n]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]


subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = ys ++ map (x:) ys
            where ys = subs xs

interleave :: a -> [a] -> [[a]]
interleave a [] = [[a]]
interleave x (y:ys) = (x:y:ys):map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))


choices :: [a] -> [[a]]
--choices xs = concat (map perms (subs xs))
choices xs = [as|ss <- subs xs,
              as <- perms ss]


solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = eval e == [n] && elem (values e) (choices ns)

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs):[(x:ls,rs)|(ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e|(ls, rs) <- split ns,
            l <- exprs ls,
            r <- exprs rs,
            e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r| o <- ops]
ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e|ns' <- choices ns,
                  e <- exprs ns',
                  eval e == [n]]
type Result = (Expr, Int)
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n)]
results ns = [res|(ls,rs) <- split ns,
              lx <- results ls,
              ry <- results rs,
              res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r, y) = [(App o l r, apply o x y)|o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e|ns' <- choices ns,
                   (e,m) <- results ns',
                   m == n]


-- 11-7.2
dropIfExist :: Eq a => a -> [a] -> [a]
dropIfExist _ [] = []
dropIfExist x (y:ys)
  | x == y = ys
  | otherwise = y:dropIfExist x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice (x:xs) ys = elem x ys && isChoice xs (dropIfExist x ys)

