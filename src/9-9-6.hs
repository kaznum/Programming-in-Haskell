type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished b = all (== 0) b

valid :: Board -> Int -> Int -> Bool
valid b r n = b !! (r - 1) >= n

move :: Board -> Int - Int -> Board
move b row num = [if row == r then n - num else n|(r,n) <- zip [1..5] b]

newline :: IO ()
newline = putChar '\n'

putBoard :: Board -> IO ()
putBoard [a, b, c, d, e] = do putRow 1 a
                              putRow 2 b
                              putRow 3 c
                              putRow 4 d
                              putRow 5 e

putRow :: Int -> Board -> IO ()
putRow row num = do putStr(show 1)
                    putStr(": ")
                    putStrLn(stars num)

stars :: Int -> IO ()
stars = concat(replicate n "* ")

getDigit :: String -> IO Int
getDigit prom = do putStr prom
                   x <- getChar
                   newline
                   if isDigit x then
                     return(ord x - ord '0')
                   else
                     do putStr "Error "
                        getDigit prom

nim :: IO ()
nim = play initial 1

play :: Board -> Int
play board player = do newline
                       putBoard board
                       if finished board then
                         do newline
                            putStr "Player "
                            putStr(show (next player))
                            putStrLn " wins"
                       else
                         do newline
                            putStr "Player "
                            putStrLn(show player)
                            r <- getDigit "Enter row number..."
                            n <- getDigit "Stars to remove..."
                            if valid board r n then
                              play (move board r n) (next player)
                            else
                              do newline
                                 putStrLn "Enter correct values"
                                 play board player

next :: Int -> Int
next 1 = 2
next 2 = 1


