module Clase03 where

factorial :: Int -> Int
factorial 1 = 1
factorial x = factorial (x - 1) * x

xor :: Bool -> Bool -> Bool
xor a b = a /= b

esMulti3 :: Int -> Bool
esMulti3 0 = True
esMulti3 1 = False
esMulti3 x = esMulti3 (abs (x-3))

fib 0 = 1
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

parteEntera :: Float -> Integer
parteEntera x | x < 1 = 0
              | otherwise = parteEntera (x-1) + 1

nMultiplo3 :: Int -> Bool
nMultiplo3 n | n < 0 = False
             | n == 0 = True
             | n == 1 = False
             | otherwise = nMultiplo3 (n - 3)

sumaImpares :: Int -> Int
sumaImpares n | n < 0 = undefined
              | n == 1 = 1
              | otherwise = sumaImpares (n-1) + ((n*2)-1)

medioFact :: Int -> Int
medioFact n | n < 0 = undefined
            | n == 1 = 1
            | n == 2 = 2
            | otherwise = n * medioFact (n-2)
{-
digitosIguales :: Int -> Bool
digitosIguales n | n < 0 = undefined
                 | a == 0 = True
                 | a == b = digitosIguales (n `div` 10)
                 | otherwise = False
                        where a = (n `div` 10) `mod` 10
                              b = (n `mod` 10)
-}


sumaDigitos :: Int -> Int
sumaDigitos n | n < 0 = undefined
              | n < 10 = n
              | otherwise = sumaDigitos (n `div` 10) + (n `mod` 10)

cantidadDigitos :: Int -> Int
cantidadDigitos n | n < 10 = 1
                  | otherwise = cantidadDigitos (n `div` 10) + 1

digitosIguales :: Int -> Bool
digitosIguales n = sumaDigitos n == n `mod` 10 * (cantidadDigitos n)

esImpar :: Int -> Bool
esImpar n | n `mod` 2 == 1 = True
          | otherwise = False

esPotencia2 :: Int -> Bool
esPotencia2 n | n == 1 = True
              | mod n 2 == 0 = esPotencia2 (n `div` 2)
              | otherwise = False

aQueExponente :: Int -> Int
aQueExponente n  | n == 1 = 0
                 | esPotencia2 n == True = aQueExponente (n `div` 2) + 1
                 | otherwise = -1

esPar :: Int -> Bool
esPar n | n `mod` 2 == 0 = True
        | otherwise = False

digitoN:: Int -> Int -> Int
digitoN x n = mod (div x (10^(n-1))) 10

esAlternado :: Int -> Bool
esAlternado n   | n < 10 = True
                | (esPar (digitoN n 1)) /= (esPar (digitoN n 2)) = esAlternado (n `div` 10)
                | otherwise = False

main =
        do
                print(esAlternado 12345678)
                print(aQueExponente 256)
                print(esPotencia2 1024)
                print(digitoN 12345 3)