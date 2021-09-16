main = do
    putStrLn "Programa para testear las funciones del TP1."
    line <- getLine
    if null line
        then main
        else do
            putStrLn "Nv"
            return ()