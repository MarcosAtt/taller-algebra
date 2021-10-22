module Clase09 where
import Clase05(mcd,mcm)
{-Algoritmos sobre enteros I-}
{-Teorema: Dados a,d enteros, existen q,r enteros / a = q*d + r-}
{-Division y modulo en naturales-}
divNat :: Int -> Int -> Int
divNat a d | a < d = 0
           | otherwise = (a-d) `divNat` d   + 1

modNat :: Int -> Int -> Int
modNat a d = a - d*(a `divNat` d)

{-Bases de numeracion-}
-- Toma un numero natural y una base. Devuelve una lista de numeros que representa el numero en base b.
digitos :: Integer -> Integer -> [Integer]
digitos 0 b = []
digitos n b | n `div` b /= 0 = {-(head (digitos (n`div`b) b)):-}(n`div`b`div`b`mod`b):(n`div`b `mod` b):(n `mod` b):[]
            | n `div` b == 0 = []
            | otherwise = []

digitosAux n b | n`div`b /= 0 = digitosAux (n`div`b) b
               | otherwise = n

--ej dig 123 7 = [2,3,4]
--   dig 480 8 = [7,4,0]
--numero :: [Integer] -> Integer -> Integer

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