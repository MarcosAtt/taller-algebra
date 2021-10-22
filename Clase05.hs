module Clase05 where
import Clase03(factorial, fib, esPar)
{- Clase 05: Recursion con funciones auxiliares. -}

prod :: Int -> Int -> Int
prod d h | d == h = d
         | otherwise = prod d (h-1) * h

sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n

{-Suma los divisores de N hasta el K -}
sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n k | k == 1 = 1
                       | n `mod` k == 0 = k + sumaDivisoresHasta n (k-1)
                       | otherwise      = sumaDivisoresHasta n (k-1)
-- Calcula el menor divisor mayor que 1.
menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n k | n == k = n
                      | n `mod` k == 0 = k
                      | otherwise = menorDivisorDesde n (k+1)

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = n == menorDivisor n

esPrimo' :: Int -> Bool
esPrimo' n = (n+1) == (sumaDivisores n)

nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n-1))

minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n | esPrimo n = n
                   | otherwise = minimoPrimoDesde (n+1)
{-
Implementar menorFactDesde :: Int -> Int que dado m ≥ 1 encuentra el mı́nimo n ≥ m
tal que n = k! para algún k.
7 Implementar mayorFactHasta :: Int -> Int que dado m ≥ 1 encuentra el máximo
n ≤ m tal que n = k! para algún k.
8 Implementar esFact :: Int -> Bool que dado n ≥ 0 decide si existe un número entero
k ≥ 0 tal que n = k! -}

menorFactDesde :: Int -> Int
menorFactDesde m = minimoFactDeHasta 1 m

minimoFactDeHasta :: Int -> Int -> Int
minimoFactDeHasta i m | factorial i >= m = factorial i
                      | otherwise = minimoFactDeHasta (i+1) m

mayorFactHasta :: Int -> Int
mayorFactHasta m = maximoFactHasta 1 m

maximoFactHasta :: Int -> Int -> Int
maximoFactHasta n m | factorial n > m = factorial (n-1)
                    | otherwise = maximoFactHasta (n+1) m

esFact :: Int -> Bool
esFact 0 = True
esFact n | mayorFactHasta n == n = True
         | otherwise = False

{-Implementar esFibonacci :: Int -> Bool que dado un número entero n ≥ 0 decide si n es un número de Fibonacci.

Implementar esSumaInicialDePrimos :: Int -> Bool que dado un número entero n ≥ 0
decide si n es igual a la suma de los m primeros números primos, para algún m.-}

esFibonacci :: Int -> Bool
esFibonacci n = esFibonacciHasta 0 n

esFibonacciHasta i n | fib i == n = True
                     | i == n = False
                     | otherwise = esFibonacciHasta (i+1) n

esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos n = terminosSumaAux n 1

terminosSumaAux n m | n == m = False
                    | sumaPrimerosMPrimos (m) == n = True
                    | otherwise = terminosSumaAux n (m+1)

sumaPrimerosMPrimos :: Int -> Int
sumaPrimerosMPrimos 1 = 2
sumaPrimerosMPrimos m = nEsimoPrimo m + sumaPrimerosMPrimos (m-1)

{- 11 - Implementar tomaValorMax :: Int -> Int -> Int que 
dado un número entero n1 ≥ 1 y un n2 ≥ n1 devuelve algún m entre n1 y n2 tal que
sumaDivisores(m) = max{sumaDivisores(i) | n 1 ≤ i ≤ n 2 }
-}
tomaValorMax :: Int -> Int -> Int
tomaValorMax n1 n2 | n1 == n2 = n1
                   | sumaDivisores n1 < sumaDivisores n2 = tomaValorMax (n1+1) n2
                   | otherwise = tomaValorMax n1 (n2-1)

{-
12 Implementar tomaValorMin :: Int -> Int -> Int que dado un número entero n 1 ≥ 1 y
un n 2 ≥ n 1 devuelve algún m entre n 1 y n 2 tal que
sumaDivisores(m) = min{sumaDivisores(i) | n 1 ≤ i ≤ n 2 }-}

tomaValorMin :: Int -> Int -> Int
tomaValorMin n1 n2 | n1 == n2 = n1
                   | sumaDivisores n1 < sumaDivisores n2 = tomaValorMin n1 (n2-1)
                   | otherwise = tomaValorMin (n1+1) n2

{-Los números naturales a y b forman un par de primos gemelos si b = a + 2 y tanto a como
b son primos. Implementar primosGem :: Int -> Int que dado n, devuelve la cantidad de
pares de primos gemelos (a, b) que verifican b ≤ n. Por ejemplo: primosGem 5 = 1 (porque
3 y 5 es un par de primos gemelos) primosGem 14 = 3 (porque 3 y 5, 5 y 7, y 11 y 13 son
tres pares de primos gemelos)
-}
primosGem :: Int -> Int
primosGem n | n < 5 = 0
            | n == 5 = 1
            | esParGem (n-2) n = 1 + primosGem (n-2)
            | otherwise = primosGem (n-1)

esParGem :: Int -> Int -> Bool
esParGem a b = (b == a+2) && esPrimo a && esPrimo b

{-
14 Conjetura de los primos gemelos: Existen infinitos pares de primos gemelos. Implementar
la función proxPrimosGem :: Int -> (Int,Int) que dado n devuelve el primer par de
gemelos (a, b) tal que a > n.-}

proxPrimosGem :: Int -> (Int,Int)
proxPrimosGem n | esParGem n (n+2) = (n,n+2)
                | otherwise = proxPrimosGem (n+1)
{-
Conjetura de Lothar Collatz: sea la siguiente definición:
| An/2      si An es par
| 3*An + 1  si An es impar

empezando A1 con cualquier entero positivo siempre se llega a 1. Por ejemplo, si A1 = 13,
obtenemos la siguiente secuencia: 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1 (9
reducciones, o sea 9 flechas).
a. Implementar largoSecuencia :: Int -> Int que dado un n > 0 devuelve la cantidad
de reducciones desde A1 = n hasta llegar a 1. Por ejemplo, largoSecuencia 13 es 9.
b. Resolver usando Haskell: ¿qué número menor a 10.000 para A1 produce la secuencia de
números más larga hasta llegar a 1? Sugerencia: usar la idea de la función del
ejercicio 11.-}

largoSecuencia :: Int -> Int
largoSecuencia n | n == 1 = 0
                 | esPar n = 1 + largoSecuencia (n `div` 2)
                 | otherwise = 1 + largoSecuencia (3*n + 1)

collatzMax :: Int -> Int -> Int
collatzMax n1 n2 | n1 == n2 = n1
                 | largoSecuencia n1 < largoSecuencia n2 = collatzMax (n1+1) n2
                 | otherwise = collatzMax n1 (n2-1)
-- Esta funcion no la pedian.
mcm :: Int -> Int -> Int
mcm a b = (a * b) `div` (mcd a b)

mcd :: Int -> Int -> Int
mcd a b | a > b = divideAmbos a b a
                       | b > a = divideAmbos a b b
                       | a == b = a

divideAmbos :: Int -> Int -> Int -> Int
divideAmbos a b i | i < 0 = 1
                  | (esDivisor i a) && (esDivisor i b) = i
                  | otherwise = divideAmbos a b (i-1)

sonCoprimos :: Int -> Int -> Bool
sonCoprimos a b = mcd a b == 1

cantidadCoprimosMenores :: Int -> Int
cantidadCoprimosMenores n = cantidadCoprimosMenoresDesde n 2

cantidadCoprimosMenoresDesde :: Int -> Int -> Int
cantidadCoprimosMenoresDesde n k | n == k = 0
                                 | sonCoprimos n k = 1 + cantidadCoprimosMenoresDesde n (k+1)
                                 | otherwise = cantidadCoprimosMenoresDesde n (k+1)

satisfaceGoldbach :: Int -> Bool
satisfaceGoldbach n = esPar n && (n > 2) && existenDosPrimosDesdeA 1 n n

existenDosPrimosDesdeA :: Int -> Int -> Int -> Bool
existenDosPrimosDesdeA a b n | not (existenDosPrimosDesde a b n) = existenDosPrimosDesdeA (a+1) b n
                             | a > b = False
                             | otherwise = True

existenDosPrimosDesde :: Int -> Int -> Int -> Bool
existenDosPrimosDesde a b n | b < a = False
                            | nEsimoPrimo a + nEsimoPrimo b == n = True
                            | otherwise = existenDosPrimosDesde a (b-1) n

{- Estas funciones ya estan incluidas en Haskell
min :: Int -> Int -> Int
min a b | a < b = a
        | otherwise = b

max :: Int -> Int -> Int
max a b | a > b = a
        | otherwise = b
-}

-- Version de Tobi, mas pro

esDivisor :: Int -> Int -> Bool
esDivisor k a = a `mod` k == 0

existeDivisorComunHasta :: Int -> Int -> Int -> Bool
existeDivisorComunHasta _ _ 1 = False
existeDivisorComunHasta a b hasta = (esDivisor hasta a) && (esDivisor hasta b) || existeDivisorComunHasta a b (hasta-1)

sonCoprimos' :: Int -> Int -> Bool
sonCoprimos' a b = not (existeDivisorComunHasta a b (min a b))

-- Refactorizacion de lo mio con booleanos como hizo tobi. La idea era exactamente la misma pero usaba un entero sin necesidad.

divideAmbos2 :: Int -> Int -> Int -> Bool
divideAmbos2 _ _ 1 = False
divideAmbos2 a b i = (esDivisor i a) && (esDivisor i b) || divideAmbos2 a b (i-1)

sonCoprimos2 :: Int -> Int -> Bool
sonCoprimos2 a b = not (divideAmbos2 a b (min a b))

--sonCoprimosHasta' :: Int -> Int -> Int -> Bool