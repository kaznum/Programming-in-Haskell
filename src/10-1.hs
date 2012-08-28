type Assoc k v = [(k,v)]
find :: Eq k => k -> Assoc k v -> v
find k t = head[v|(k', v) <- t, k' == k]
