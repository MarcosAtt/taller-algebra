module Clase08 where
--import Clase07
--type Set a = [a]

combinatorio 0 0 = 1
combinatorio n 0 = 1
combinatorio n 1 = n
combinatorio n k | n == k = 1
                 | otherwise =  (combinatorio (n-1) k) + (combinatorio (n-1) (k-1))

type Set a = [a]

vacio :: Set a
vacio = []

agregar :: Eq a => a -> Set a -> Set a
agregar n c | n `elem` c = c
            | otherwise  = n:c

union :: Eq a => Set a -> Set a -> Set a
union [] ys     = ys
union (x:xs) ys = union xs (agregar x ys)

variaciones :: Set Int -> Int -> Set [Int]
variaciones c 0 = [[]]
variaciones c k = agregarElementosALista c (variaciones c (k-1))

agregarElementoAdelante :: Int -> Set [Int] -> Set [Int]
agregarElementoAdelante x [] = []
agregarElementoAdelante x (ys:yss) = agregar (x:ys) (agregarElementoAdelante x yss)

agregarElementosALista :: Set Int -> Set [Int] -> Set [Int]
agregarElementosALista [] _ = []
agregarElementosALista (x:xs) c = union (agregarElementoAdelante x c) (agregarElementosALista xs c)

{-dados una lista l, un número n y una
posición i (contando desde 1) devuelva una lista en donde se insertó
 n en la posición i de l y los elementos siguientes corridos en una posición.-}

insertarEn :: [Int] -> Int -> Int -> [Int]
insertarEn xs n i | i == 1 = n:xs
                  | otherwise = (head xs) : (insertarEn (tail xs) n (i-1))

insertarEnCadaPos :: [Int] -> Int -> Int -> Set [Int]
insertarEnCadaPos xs c 1 = agregar (insertarEn xs c 1) vacio
insertarEnCadaPos xs c i = agregar (insertarEn xs c i) (insertarEnCadaPos xs c (i-1))

insertarEnCadaPosicionDeTodasLasListas :: Set [Int] -> Int -> Set [Int]
insertarEnCadaPosicionDeTodasLasListas [] c = []
insertarEnCadaPosicionDeTodasLasListas (xs:xss) c = (insertarEnCadaPos xs c (length xs + 1)) `union`
                                                    (insertarEnCadaPosicionDeTodasLasListas xss c)

permutaciones :: Set Int -> Set [Int]
permutaciones [] = [[]]
permutaciones (c:cs) = insertarEnCadaPosicionDeTodasLasListas (permutaciones cs) c

{-Todas las formas de ubicar n bolitas numeradas en k cajas.
bolitasEnCajas :: Int -> Int -> Set [Int]
Notar que el elemento i de cada sublista representa el n ́umero de caja donde fue a parar la
bolita i .-}

{-2 Todas las formas de ubicar n bolitas numeradas en k cajas tal que la primera caja nunca
est ́e vac ́ıa-}

{-Todas las listas ordenadas de k n ́umeros distintos tomados del conjunto {1,...,n}.
4 Todas las sucesiones de los caracteres ’a’ y ’b’ de longitud n y m respectivamente.
5 Todas las sucesiones de ’a’, ’b’ y ’c’ de longitud n, m y k respectivamente.
6 Implementar una funci ́on
subconjuntos :: Set Int -> Int -> Set (Set Int) que dados un conjunto de enteros
y un entero k , genera todos los subconjuntos de k elementos del conjunto pasado por
par ́ametro.-}
