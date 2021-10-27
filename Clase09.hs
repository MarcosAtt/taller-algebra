module Clase09 where
import Clase05(mcd,mcm)
import Clase08()
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
digitos :: Integer -> Integer -> [Integer]
digitos 0 b = 0
digitos n b = (n `mod` b):





--digitosAux :: Integer -> Integer -> Integer

























{-
digitosAux n b | n`div`b /= 0 = digitosAux (n`div`b) b
               | otherwise = n

Definir la funci ́on numero :: [Integer] -> Integer -> Integer que, dada la
representaci ́on por listas de n ≥0 en base b y la base b >1, retorne n.-}

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