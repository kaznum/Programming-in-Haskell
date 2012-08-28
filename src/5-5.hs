import Char

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i|(x',i) <- zip xs [0..n], x == x']
                 where n = length xs - 1

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr(ord 'a' + n)

shift :: Int -> Char -> Char
shift n c |isLower c = int2let((let2int(c) + n) `mod` 26)
          |otherwise = c


encode :: Int -> String -> String
encode n xs = [shift n x|x <- xs]

count :: Char -> String -> Int
count c xs = length [1|x <- xs, c == x]

lowers :: String -> Int
lowers xs = length [1|x <- xs, isLower x]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count c xs) m|c <- ['a'..'z']]
  where m = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr xs ys = sum [(o - e)^2 / e | (o, e) <- zip xs ys]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

table = [8.2,
         1.5,
         2.8,
         4.3,
         12.7,
         2.2,
         2.0,
         6.1,
         7.0,
         0.2,
         0.8,
         4.0,
         2.4,
         6.7,
         7.5,
         1.9,
         0.1,
         6.0,
         6.3,
         9.1,
         2.8,
         1.0,
         2.4,
         0.2,
         2.0,
         0.1] :: [Float]

crack :: String -> String
crack xs = encode(-factor) xs
           where
             factor = head (positions (minimum chitab) chitab)
             chitab = [chisqr (rotate n table') table| n <- [0..25]]
             table' = freqs xs