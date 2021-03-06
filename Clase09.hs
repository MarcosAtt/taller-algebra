module Clase09 where
import Clase08(insertarEn)
import Clase06(listaDivisores,maximo)
import Clase07(intereseccion)

{-Algoritmos sobre enteros I-}
{-Teorema: Dados a,d enteros, existen q,r enteros / a = q*d + r-}
{-Division y modulo en naturales-}
divNat :: Int -> Int -> Int
divNat a d | a < d = 0
           | otherwise = (a-d) `divNat` d   + 1

modNat :: Int -> Int -> Int
modNat a d = a - d*(a `divNat` d)

{-Bases de numeracion-}
{- dados n ≥ 0 y b > 1, retorne su representacion por listas en base b.
   1537 = (1537)_10 = [7,3,5,1]_10
   29   = (11101)_2 = [1,0,1,1,1]_2
   1024 = (400)16 = [0,0,4]16
   255 = (FF )16  = [15,15]16
-}
digitos :: Int -> Int -> [Int]
digitos 0 b = []
digitos n b = digitosAux [] n b

digitosAux :: [Int] -> Int -> Int -> [Int]
digitosAux l n b | (n `div` b) == 0 = insertarEn l n 1
                 | otherwise        = digitosAux (insertarEn l (n `mod` b) 1) (n `div` b) b

{-
Definir la funcion numero :: [Integer] -> Integer -> Integer que, dada la
representaci ́on por listas de n ≥0 en base b y la base b >1, retorne n.-}

--ej dig 123 7 = [2,3,4]
--   dig 480 8 = [7,4,0]
numero :: [Int] -> Int -> Int
numero (x:xs) b | xs == [] = x*b^0
                | otherwise = x*b^(length (x:xs) -1) + numero xs b

mcdDef :: Int -> Int -> Int
mcdDef a 0 = abs a
mcdDef 0 b = abs b
mcdDef a b = maximo (intereseccion (listaDivisores a) (listaDivisores b))
-- Este es eficiente, usa el algoritmo de Euclides
mcd :: Int -> Int -> Int
mcd a 0 = abs a
mcd 0 b = abs b
mcd a b | (a == 1) || (b == 1) = 1
        | abs a >= abs b = mcd (a `mod` b) b
        | otherwise = mcd (b `mod` a) a

mcm :: Int -> Int -> Int
mcm a b = (abs (a*b)) `div` (mcd a b)

-- emcd a b = ((a:b), s, t)
emcd :: Int -> Int -> (Int, Int, Int)
emcd 0 b = (b, 0, 1)
emcd a b = (g, t - (b `div` a) * s, s)
        where (g, s, t) = emcd (b `mod` a) a


-- A reimplementar
solEcLineal :: (Int,Int,Int) -> (Int,Int)
--solEcLineal (c,a,b) | mod c (mcd (a,b)) /= 0 = undefined
solEcLineal (c,a,b) = (k*s,k*t)
                    where
                        k = div c (mcd a b)
                        (s,t) = mcdExt (a,b)

mcdExt :: (Int,Int) -> (Int,Int)
mcdExt (a,0) = (1,0)
mcdExt (a,b) = (t,s-q*t)
            where
            q = a `div` b
            (s,t) = mcdExt (b, (a `mod` b))