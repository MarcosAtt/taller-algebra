import Tp1

-- Funciones de Testeo: toman una funcion la evaluan al segundo argumento y la comparan con el tercero. Si la prueba fue exitosa devuelven True.
testFuncionBool :: (Integer -> Bool) -> Integer -> Bool -> Bool
testFuncionBool funcion x c = funcion x == c

testFuncionInteger :: (Integer -> Integer) -> Integer -> Integer -> Bool
testFuncionInteger funcion x c = funcion x == c

testFuncionParInteger :: (Integer -> (Integer,Integer)) -> Integer -> (Integer,Integer) -> Bool
testFuncionParInteger funcion x c = funcion x == c

main = do
    putStrLn "Programa para testear las funciones del TP1."
    putStrLn "--------1: Test esSumaDeDosCubos --------\n"
    putStrLn (show (testFuncionBool esSumaDeDosCubos 88 False))
    putStrLn (show (testFuncionBool esSumaDeDosCubos 2 True))
    putStrLn (show (testFuncionBool esSumaDeDosCubos 79625 True))
    putStrLn (show (testFuncionBool esSumaDeDosCubos 97187584969377 True))
    putStrLn (show (testFuncionBool esSumaDeDosCubos 97187584969378 False))

    putStrLn "--------2: Test descomposicionCubos --------\n"
    putStrLn (show (testFuncionParInteger descomposicionCubos 9 (1,2)))
    putStrLn (show (testFuncionParInteger descomposicionCubos 1729 (1,12)))
    putStrLn (show (testFuncionParInteger descomposicionCubos 4104 (2,16)))
    putStrLn (show (testFuncionParInteger descomposicionCubos 20683 (10,27)))

    putStrLn "--------3: Test cantidadDeFormas --------\n"
    putStrLn (show (testFuncionInteger cantidadDeFormas 88 0))
    putStrLn (show (testFuncionInteger cantidadDeFormas 4104 2))
    putStrLn (show (testFuncionInteger cantidadDeFormas 6963472309248 4))
    putStrLn (show (testFuncionInteger cantidadDeFormas 9 1))

    putStrLn "--------4: Test especialDesde --------\n"
    putStrLn (show (testFuncionInteger especialDesde 5 1729))
    putStrLn (show (testFuncionInteger especialDesde 1729 1729))
    putStrLn (show (testFuncionInteger especialDesde 1730 4104))
    putStrLn (show (testFuncionInteger especialDesde 8000 13832))
    putStrLn (show (testFuncionInteger especialDesde 87539299 87539319))

    putStrLn "--------5: Test especialNumero --------\n"
    putStrLn (show (testFuncionInteger especialNumero 1 1729))
    putStrLn (show (testFuncionInteger especialNumero 4 20683))
    putStrLn (show (testFuncionInteger especialNumero 12 110808))

    putStrLn "--------6: Test esMuyEspecial --------\n"
    putStrLn (show (testFuncionBool esSumaDeDosCubos 165464 False))
    putStrLn (show (testFuncionBool esSumaDeDosCubos 1729 True))
    putStrLn (show (testFuncionBool esSumaDeDosCubos 3 False))
    putStrLn (show (testFuncionBool esSumaDeDosCubos 20683 True))
    putStrLn (show (testFuncionBool esSumaDeDosCubos 805688 True))

    line <- getLine
    if null line
        then main
        else do
            putStrLn "- Terminando..."
            return ()