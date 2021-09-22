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

buscarElementosIguales :: (Eq a)=> [a] -> Int
buscarElementosIguales [] = 0
buscarElementosIguales l  | (head l) == (head (tail l)) = 1 + buscarElementosIguales (tail l)
                          | otherwise = 1

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