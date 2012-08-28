readLine :: IO String
readLine xs = do x <- getChar
              case x of
                '\n' -> return xs
                '\DEL' -> if null xs then
                            readLine xs
                          else
                            do putStr "\ESC[1D\ESC[1D"
                               readLine(init xs)
                _ -> readLine(xs ++ [x])
