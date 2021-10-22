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
