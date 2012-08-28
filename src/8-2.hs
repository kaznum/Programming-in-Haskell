type Parser a = String -> [(a, String)]

return :: a -> Parser a
return v = \inp -> [(v, inp)]

failure :: Parser a
failure = \inp -> []

item :: Parser Char
item = \inp -> case inp of
  [] -> []
  (x:xs) -> [(x,xs)]

parse :: Parser a -> String -> [(a,String)]
parse p inp = p inp

--parse (return 1) "abc"
--parse (failure) "abc"
--parse item "abc"

