module Clase07 where
import Clase06(pertenece)
{-Clase 7: Conjuntos-}
type Set a = [a]

vacio :: Set Int
vacio = []

agregar :: Int -> Set Int -> Set Int
agregar n c | pertenece n c = c
            | otherwise     = n:c

incluido :: Set Int -> Set Int -> Bool
incluido [] c = True
incluido (x:xs) c = pertenece x c && incluido xs c

iguales :: Set Int -> Set Int -> Bool
iguales c1 c2 = incluido c1 c2 && incluido c2 c1

 

intereseccion :: Set Int -> Set Int -> Set Int
intereseccion [] _ = []
intereseccion (x:xs) c2 | pertenece x c2 = x:(intereseccion xs c2)
                        | otherwise      =    intereseccion xs c2

diferencia :: Set Int -> Set Int -> Set Int
diferencia [] _ = []
diferencia a [] = a
diferencia (x:xs) c2 | not (pertenece x c2) = x:(diferencia xs c2)
                     | otherwise            =    diferencia xs c2

diferenciaSimetrica :: Set Int -> Set Int -> Set Int
diferenciaSimetrica c1 c2 = union (diferencia c1 c2) (diferencia c2 c1)

agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC xs xss | perteneceC xs xss = xss
                | otherwise         = xs:xss

perteneceC :: Set Int -> Set (Set Int) -> Bool
perteneceC xs [] = False
--perteneceC [] xs = True
perteneceC xs (ys:yss) = iguales xs ys || perteneceC xs yss

agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos x [] = []
agregarATodos x (c:cs) = agregarC (agregar x c) (agregarATodos x cs)

unionC :: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
unionC [] y = y
unionC x [] = x
unionC (xs:xss) (ys:yss) | not (perteneceC xs yss) = xs:(unionC xss (ys:yss))
                         | otherwise               = unionC xss (ys:yss)

partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (x:xs) = unionC (partes xs) (agregarATodos x (partes xs))

partesN :: Int -> Set (Set Int)
partesN 0 = [[]]
partesN n = partesNAux n 1 []

partesNAux :: Int -> Int -> Set Int -> Set (Set Int)
partesNAux n k c | k == n                = partes (k:c)
                 | otherwise             = unionC (partes c) (partesNAux n (k+1) (k:c))
 


