{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Clase06 where
-- Clase sobre listas
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria n  = head n + sumatoria (tail n)

sumatoria' :: [Int] -> Int
sumatoria' [] = 0
sumatoria' (n:ns) = n + sumatoria ns

longitud :: [Int] -> Int
longitud [] = 0
longitud l  = 1 + longitud (tail l)

pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece n l = (n == head l) || pertenece n (tail l)

pertenece' :: Int -> [Int] -> Bool
pertenece' _ [] = False
pertenece' n (x:xs) = (n == x) || pertenece' n xs

primerMultiplode45345 :: [Int] -> Int
primerMultiplode45345 [] = 0
primerMultiplode45345 l | mod (head l) 45345 == 0 = (head l)
                        | otherwise = primerMultiplode45345 (tail l)

comprimir :: (Eq a)=> [a] -> [(a,Int)]
comprimir (x:xs) | xs == []     = [(x,1)]
                 | x /= head xs = (x,1):comprimir (xs)
                 | otherwise    = (x,1+ snd (head (comprimir xs))):(tail (comprimir xs))

-- agrega a la lista l el elemento e n veces al principio
agregarNvecesAlprincipio :: Int -> a -> [a] -> [a]
agregarNvecesAlprincipio 0 e l = l
agregarNvecesAlprincipio n e l = e:(agregarNvecesAlprincipio (n-1) e l)

-- p = (e,n)
descomprimir [] = []
descomprimir (p:ps) = agregarNvecesAlprincipio (snd p) (fst p) (descomprimir ps)

productoria :: [Int] -> Int
productoria [] = 0
productoria (x:xs) | xs == [] = x
                   | otherwise = x + productoria xs

sumarN :: Int -> [Int] -> [Int]
sumarN n (x:xs) | xs == [] = [(x+n)]
                | otherwise = (x+n):(sumarN n (xs))

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero (x:xs) = sumarN (x) (xs)

ultimo :: [Int] -> Int
ultimo [] = 0
ultimo (x:xs) | xs == [] = x
              | otherwise = ultimo xs

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo (x:xs) = sumarN (ultimo xs) (x:xs)

pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) | x `mod` 2 == 0 = x:(pares xs)
             | otherwise = pares xs

quitar :: Int -> [Int] -> [Int]
quitar n (x:xs) | xs == [] && x /= n = [x]
                | xs == [] = []
                | x == n = sumarN 0 (xs)
                | otherwise = (x):(quitar n (xs))

quitarTodas :: Int -> [Int] -> [Int]
quitarTodas _ [] = []
quitarTodas n (x:xs) | xs == [] = []
                     | pertenece n (x:xs) = quitarTodas n (quitar n (x:xs))
                     | otherwise = x:xs

hayRepetidos :: [Int] -> Bool
hayRepetidos (x:xs) | xs == [] = False
                    | pertenece x xs = True
                    | otherwise = hayRepetidos xs

eliminarRepetidosAlFinal :: [Int] -> [Int]
eliminarRepetidosAlFinal [] = []
eliminarRepetidosAlFinal (x:xs) | xs == [] = [x]
                                | hayRepetidos (x:xs) = x:(eliminarRepetidosAlFinal (quitarTodas x xs))
                                | otherwise = x:xs

eliminarRepetidosAlInicio :: [Int] -> [Int]
eliminarRepetidosAlInicio (x:xs) | xs == [] = [x]
                                 | pertenece x xs = eliminarRepetidosAlInicio xs
                                 | otherwise      = x:(eliminarRepetidosAlInicio xs)

maximo :: [Int] -> Int
maximo (x:xs) = maximoAux (eliminarRepetidosAlFinal (x:xs))

maximoAux :: [Int] -> Int
maximoAux (x:xs) | xs == [] = x
                 | x < (head xs) = maximoAux xs
                 | otherwise = maximoAux (x:(tail xs))

minimo :: [Int] -> Int
minimo (x:xs) = minimoAux (eliminarRepetidosAlFinal (x:xs))

minimoAux :: [Int] -> Int
minimoAux (x:xs) | xs == [] = x
                 | x > (head xs) = minimoAux xs
                 | otherwise = minimoAux (x:(tail xs))

ordenar :: [Int] -> [Int]
ordenar l | l == [] = []
          | otherwise = (minimo l):(ordenar (quitar (minimo l) l))

sinUltimo :: [Int] -> [Int]
sinUltimo [] = []
sinUltimo (x:xs) | xs == [] = []
                 | otherwise = x:(sinUltimo xs)

reverso :: [Int] -> [Int]
reverso l | l == [] = []
          | otherwise = (ultimo l):(reverso (sinUltimo l))

concatenar :: [Int] -> [Int] -> [Int]
concatenar _ [] = []
concatenar a b | a == []   = (head b):(concatenar a (tail b))
               | otherwise = (head a):(concatenar (tail a) b)

zipi :: [a] -> [b] -> [(a,b)]
zipi [] b = []
zipi a [] = []
zipi a b = ((head a),(head b)):(zipi (tail a) (tail b))

listaDivisores :: Int -> [Int]
listaDivisores n = listaDivisoresAux n 1

listaDivisoresAux :: Int -> Int -> [Int]
listaDivisoresAux n k | (k > (n `div` 2)) = [n]
                      | n `mod` k == 0     =  k:(listaDivisoresAux n (k+1))
                      | otherwise          =     listaDivisoresAux n (k+1)
