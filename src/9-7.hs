import Char
import System.IO

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
    (symbol "+" >>== \_ ->
     expr >>== \e ->
     return' (t + e)) +++ return' t


term :: Parser Int
term =
  factor >>== \f ->
    (symbol "*" >>== \_ ->
    term >>== \t ->
    return' (f * t)) +++ return' f

factor :: Parser Int
factor =
  (symbol "(" >>== \_ ->
  expr >>== \e ->
  symbol ")" >>== \_ ->
  return' e) +++ natural


getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c


box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]


seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn(a:as) = do a
                seqn as
type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr (xs)


buttons :: [Char]
buttons = standard ++ extra
          where
            standard = "qcd=123+456-789*0()/"
            extra = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = seqn [ writeat (1, y) xs| (y, xs) <- zip [1..13] box]

display :: String -> IO ()
display xs = writeat (3, 2) (reverse (take 13 (reverse ("              " ++ xs))))

calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             writeat (1,1) ("Input: " ++ [c])
             if elem c buttons then
               process c xs
             else
               do beep
                  calc xs

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

process :: Char -> String -> IO ()
process c xs
  |elem c "qQ\ESC" = quit
  |elem c "dD\BS\DEL" = delete xs
  |elem c "=\n" = eval xs
  |elem c "cC" = clear
  |otherwise = press c xs


quit :: IO()
quit = goto(1,14)

delete :: String -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
  [(n, "")] -> calc (show n)
  _ -> do beep
          calc xs

clear :: IO ()
clear = calc ""

press :: Char -> String -> IO ()
press c xs = calc(xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear

width :: Int
width = 20
height :: Int
height = 20

type Board = [Pos]
glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

showcells :: Board -> IO ()
showcells b = seqn [ writeat p "o" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x, y) = map wrap [(x-1, y-1), (x,y-1), (x+1,y-1),
                           (x-1, y), (x+1,y),
                           (x-1, y+1), (x,y+1), (x+1,y+1)]

wrap :: Pos -> Pos
wrap (x, y) = (((x-1) `mod` width) + 1,
               ((y-1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p| p <- b,
               elem (liveneighbs b p) [2,3]]

-- births :: Board -> [Pos]
-- births b = [(x,y)| x <- [1..width], y <- [1..height],
--             isEmpty b (x,y),
--             (liveneighbs b (x,y)) == 3]
births :: Board -> [Pos]
births b = [p|  p <- (rmdups . concat . map neighbs) b,
            isEmpty b p,
            (liveneighbs b p) == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (p:ps) = p:rmdups(filter (/= p) ps)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do cls
            showcells b
            wait 50000
            life (nextgen b)

wait :: Int -> IO ()
wait n = seqn[return()| _ <- [1..n]]



