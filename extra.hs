import Clase02(factorial)
funA t = ((3-4*t),(2-3*t))

funB t = ((1-(t^2)),(t-2))

funC t = ((t^2+t),(t^2-t))

funD t = ((t^2),(t^3-4*t))

-- Sea An una sucesion
sucA 0 = 1
sucA 1 = 3
sucA 2 = 2
sucA 3 = (-1)
sucA 4 = (-3)
sucA 5 = (-2)
sucA n = sucA (n `mod` 6)

sumSucA 0 = 1
sumSucA x = sucA (x) + sumSucA (x-1)

ej13 :: Int -> Bool
ej13 n = ((n^2) + 1) < 2^n
-- Calcula el combinatorio. Funciona para numero chicos.
numeroCombinatorio :: (Int, Int) -> Int
numeroCombinatorio (n,k) = factorial(n) `div` (factorial(k)*factorial(n-k))

get1 (a,_,_,_,_) = a
get2 (_,a,_,_,_) = a
get3 (_,_,a,_,_) = a
get4 (_,_,_,a,_) = a
get5 (_,_,_,_,a) = a

testFuncionBool' :: (Integer -> Bool) -> Integer -> Bool -> ((Integer -> Bool), Integer, Bool, Bool, Bool)
testFuncionBool' funcion valorEntrada provisto = (funcion, valorEntrada, cuantoDa, provisto, paso)
                            where cuantoDa = funcion valorEntrada
                                  paso = cuantoDa == provisto
