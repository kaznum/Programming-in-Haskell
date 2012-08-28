all' p = and . map p

any' p = or . map p

takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x:takeWhile p xs
                 | otherwise = []

dropWhile' _ [] = []
dropWhile' p (x:xs) | p x = dropWhile p xs
                  | otherwise = (x:xs)