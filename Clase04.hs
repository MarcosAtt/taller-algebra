import Clase03(factorial)
sumatoriaN :: Int -> Int
sumatoriaN 0 = 0
sumatoriaN n = n + sumatoriaN (n-1)

sumatoriaN' :: Int -> Int
sumatoriaN' n = n*(n+1) `div` 2

f1 :: Int -> Int
f1 0 = 0
f1 n = 2^n + f1(n-1)

sumaQaLaN :: Int -> Int -> Int
sumaQaLaN 0 _ = 0
sumaQaLaN n q = q^n + sumaQaLaN (n-1) q

f3 :: Int -> Int -> Int
f3 n q = sumaQaLaN (2*n) q

f4 :: Int -> Int -> Int
f4 n q = (sumaQaLaN (2*n) q) - (sumaQaLaN (n-1) q)

eAprox :: Int -> Float
eAprox 0 = 1
eAprox n = 1 / (fromIntegral (factorial (n))) + eAprox(n-1)
{-fromIntegral convierte Int en Float-}

eAprox' :: Integer -> Float
eAprox' 0 = 1
eAprox' n = 1

e :: Float
e = eAprox 10

f5 :: Int -> Int -> Int
f5 0 m = 0
f5 n m = sumaQaLaN m n + f5 (n-1) m

f2 :: Int -> Float -> Float
f2 0 _ = 0
f2 n q = q^n + f2 (n-1) q

sumaPotencias :: Float -> Int -> Int -> Float
sumaPotencias q n 0 = 0
sumaPotencias q n m = sumaPotencias q n (m-1) + q^m*(f2 n q)

sumaRacionales :: Int -> Int -> Float
sumaRacionales n 0 = 0
sumaRacionales n m = (sumaRacionales n (m-1)) + (fromIntegral (sumatoriaN n)) / (fromIntegral m)
{-Conclusion: cuando hay que sumar algo, escribir todos los terminos de la suma y buscar la invocacion de la misma funcion que estoy definiendo pero con valores mas cercanos al caso base-}