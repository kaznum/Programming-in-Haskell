chop8 = unfold (null) (take 8) (drop 8)
map f = unfold (null) f . head (tail)
iterate f = unfold (const False) id f


