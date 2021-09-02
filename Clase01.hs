module Clase01 where

g x y z = x + y + z * z

doble x = x * 2

suma x y = x + y

normaVectorial x1 x2 = sqrt (x1*x1 + x2*x2)

funcionConstante8 x = 8

f0 n | n == 0 = 1
     | otherwise = 0

signo n | n > 0 = 1
        | n == 0 = 0
        | n < 0 = (-1)

maximo :: Int -> Int -> Int 
maximo x y | x < y = y
           | otherwise = x

f1 n | n >= 3 = 5

f2 n | n >= 3 = 5
     | n <= 1 = 8

f3 n | n >= 3 = 5
     | n == 2 = undefined
     | otherwise = 8

f4 n | n >= 3 = 5
     | n <= 9 = 7

f5 n | n <= 9 = 7
     | n >= 3 = 5

f 0 = 1
f n = 0

cantidadDeSoluciones b c | d > 0 = 2
                         | d == 0 = 1
                         | otherwise = 0
                         where d = b^2 - 4*c

absoluto :: Int -> Int
absoluto n | n < 0 = -n
           | otherwise = n

maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y = maximo (absoluto x) (absoluto y)

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z = maximo (maximo x y) z

algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y | x == 0 = True
              | y == 0 = True
              | otherwise = False

algunoEs0pm :: Float -> Float -> Bool
algunoEs0pm 0 _ = True
algunoEs0pm _ 0 = True
algunoEs0pm _ _ = False

ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y | x == 0 && y == 0 = True
              | otherwise = False
 
ambosSon0pm :: Float -> Float -> Bool
ambosSon0pm 0 0 = True
ambosSon0pm _ _ = False

esMultiploDe :: Int -> Int -> Bool
esMultiploDe a b = mod b a == 0

digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

digitoDecenas :: Int -> Int
digitoDecenas x = digitoUnidades (div x 10)

natacion :: Int -> Bool
natacion x = (dia == 1 || dia == 6) && x /= 29
            where dia = mod x 7

digitoN:: Int -> Int -> Int
digitoN x n = mod (div x (10^(n-1))) 10

