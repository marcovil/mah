main = do s <- getLine
          putString s
          putChar '\n'

getLine2 :: IO String
getLine2 = do c <- getChar
              if c == '\n'
                then return ""
                else do l <- getLine2
                        return (c:l)

putString :: String -> IO ()
putString [] = return ()
putString (x:xs) = do putChar x
                      putString xs
