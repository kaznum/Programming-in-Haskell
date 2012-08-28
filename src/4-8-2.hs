safetail :: [a] -> [a]
safetail xs = if null xs then xs else tail xs

safetail' :: [a] -> [a]
safetail' xs |null xs = xs
             | otherwise = tail xs

safetail''  :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs
