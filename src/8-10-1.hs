import Char

type Parser a = String -> [(a, String)]

return' :: a -> Parser a
return' v = \inp -> [(v, inp)]

failure :: Parser a
failure = \inp -> []

item :: Parser Char
item = \inp -> case inp of
  [] -> []
  (x:xs) -> [(x,xs)]

parse :: Parser a -> String -> [(a,String)]
parse p inp = p inp

(>>==) :: Parser a -> (a -> Parser b) -> Parser b
p >>== f = \inp ->
  case parse p inp of
    [] -> []
    [(v,out)] -> parse (f v) out

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
  [] -> parse q inp
  [(v, out)] -> [(v,out)]

sat :: (Char -> Bool) -> Parser Char
sat p = item >>== \x ->
  if p x then return' x else failure

char :: Char -> Parser Char
char x = sat (==x)

digit :: Parser Char
digit = sat isDigit

string :: String -> Parser String
string [] = return' []
string (x:xs) =
  char x >>== \_ ->
  string xs >>== \_ ->
  return' (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return' []
many1 :: Parser a -> Parser [a]
many1 p =
  p >>== \v ->
  many p >>== \vs ->
  return' (v:vs)

space :: Parser ()
space =
  many(sat isSpace) >>== \x ->
  return' ()

nat :: Parser Int
nat =
  many1 digit >>== \xs ->
  return' (read xs)

token :: Parser a -> Parser a
token p =
  space >>== \x ->
  p >>== \v ->
  space >>== \y ->
  return' v

symbol :: String -> Parser String
symbol xs = token(string xs)

natural :: Parser Int
natural = token nat


p :: Parser [Int]
p = symbol "[" >>== \_ ->
  natural >>== \n ->
  many(symbol "," >>== \_ -> natural) >>== \ns ->
  symbol "]" >>== \_ ->
  return' (n:ns)

expr :: Parser Int
expr =
  term >>== \t ->
    (
      symbol "+" >>== \_ ->
       expr >>== \e ->
       return' (t + e)
    ) +++ (
      symbol "-" >>== \_ ->
       expr >>== \e ->
       return' (t - e)
    ) +++ return' t

term :: Parser Int
term =
  ex >>== \f ->
    (symbol "*" >>== \_ ->
      term >>== \t ->
      return' (f * t)
    ) +++ (
      symbol "/" >>== \_ ->
       term >>== \t ->
       return' (f `div` t)
    ) +++ return' f

ex :: Parser Int
ex =
  factor >>== \f ->
    (symbol "^" >>== \_ ->
      factor >>== \t ->
      return' (f ^ t)
    ) +++ return' f

factor :: Parser Int
factor =
  (symbol "(" >>== \_ ->
  expr >>== \e ->
  symbol ")" >>== \_ ->
  return' e) +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
  [(n, [])] -> n
  [(_,out)] -> error("unused input " ++ out)
  [] -> error "invalid input"

int :: Parser Int
int =
  (char '-' >>== \_ ->
  nat >>== \n ->
  return' (-n)) +++ nat

comment :: Parser ()
comment =
  string "--" >>== \_ ->
  many(sat (/= '\n')) >>== \_ ->
  return' ()
