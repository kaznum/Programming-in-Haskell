-- True || True = True
-- True || False = True
-- False || True = True
-- False || False = False

-- True || _ = True
-- _ || b = b

-- False || b = b
-- _ || _ = True

b || c | b == c = b
       | otherwise = True