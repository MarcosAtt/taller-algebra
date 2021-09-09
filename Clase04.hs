import Clase03(factorial, esPar)

sumatoria :: Int -> Int
sumatoria 0 = 0
sumatoria n = n + sumatoria (n-1)

sumatoria' :: Int -> Int
sumatoria' n = n*(n+1) `div` 2

f1 :: Int -> Int
f1 0 = 0
f1 n = 2^n + f1(n-1)

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
sumaRacionales n m = (sumaRacionales n (m-1)) + (fromIntegral (sumatoria n)) / (fromIntegral m)
{-Conclusion: cuando hay que sumar algo, escribir todos los terminos de la suma y buscar la invocacion de la misma funcion que estoy definiendo pero con valores mas cercanos al caso base-}

sumaQaLaN :: Int -> Int -> Int
sumaQaLaN 0 _ = 0
sumaQaLaN n q = q^n + sumaQaLaN (n-1) q

g1 :: Int -> Int -> Int
g1 i n  | n == i = i^i
        | otherwise = i^n + g1 i (n-1)

sumatoriaExp :: Int -> Int -> Int
sumatoriaExp 0 m = 0
sumatoriaExp n m = n^m + sumatoriaExp (n-1) m

g2 :: Int -> Int
g2 1 = 1
g2 n = sumaQaLaN n n + sumatoriaExp (n-1) n + g2 (n-1)

g3 :: Int -> Int
g3 0 = 0
g3 n | esPar n = 2^n + g3 (n-2)
     | otherwise = g3 (n-1)
-- Suma todos los naturales <= n con digitos iguales.
7
Implementar una función que dado un n, sume todos los números naturales menores o
iguales que n que tengan todos los dı́gitos iguales.
sumaDigitosIguales :: Int -> Int
sumaDigitosIguales | n == 0 = 0
                   | n < 10 = n + sumaDigitosIguales (n-1)
                   | 

-- 1 Cumple propiedad de gauss.

sumasGauss :: Int -> Bool
sumasGauss n = sumatoria n == sumatoria' n

-- 2 Suma de cuadrados por constantes.

sumaCuadrados :: Int -> Int
sumaCuadrados 0 = 0
sumaCuadrados n = n^2 + sumaCuadrados (n-1)

sumaConstante :: Int -> Int
sumaConstante 0 = 0
sumaConstante n = 7* n^2 + sumaConstante (n-1)

constantesSumas :: Int -> Bool
constantesSumas n = (sumaConstante (n)) == (7 * sumaCuadrados n)

-- 3 Producto de sumas y sumas de productos

productoSumas :: Int -> Int -> Int
productoSumas n 1 = columna n 1
productoSumas n m = productoSumas n (m-1) + columna n m

columna :: Int -> Int -> Int
columna 0 m = 0
columna n m = columna (n-1) m + (2^n * 3^m)

sumaProd :: Int -> Int -> Bool
sumaProd 0 0 = True
sumaProd n m = (productoSumas n m) == ((sumaQaLaN n 2)*(sumaQaLaN m 3))