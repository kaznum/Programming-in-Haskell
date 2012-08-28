import Char

type Bit = Int
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin(n `div` 2)

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

make8 :: [Bit] -> [Bit]
make8 xs = take 8 (xs ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin . ord)
addParity :: [Bit] -> [Bit]
addParity xs = xs ++ [parity xs]
--addParity xs = xs

parity :: [Bit] -> Bit
parity xs = length(filter ((==) 1) xs) `mod` 2

decode :: [Bit] -> String
decode = map (chr . bin2int . check ) . chop9


chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

check :: [Bit] -> [Bit]
check bits | length(filter ((==) 1) bits) `mod` 2 == 0 = take 8 bits
           | otherwise = error "hello"
