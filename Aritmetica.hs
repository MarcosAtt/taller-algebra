module Aritmetica where
import Tipos
import Data.Tuple
import Data.Bits


--(1)
emcd :: Int -> Int -> (Int, Int, Int)
emcd 0 b = (b, 0, 1)
emcd a b = (g, t - (b `div` a) * s, s)
        where (g, s, t) = emcd (b `mod` a) a

--(2)
siguienteNumeroMayorN :: Set Integer -> Integer -> Integer
siguienteNumeroMayorN [] n = n
siguienteNumeroMayorN (x:xs) n | x > n = x
                               | otherwise = siguienteNumeroMayorN xs n

eliminarMultiplosLista :: Set Integer -> Integer -> Set Integer
eliminarMultiplosLista [] _ = []
eliminarMultiplosLista (x:xs) n | (x `mod` n == 0) && (x > n) = eliminarMultiplosLista xs n
                                | otherwise = x:(eliminarMultiplosLista xs n)

cribaAux :: Set Integer -> Integer -> Set Integer
cribaAux l n | proximoPrimo == n = l
             | otherwise = cribaAux (eliminarMultiplosLista l proximoPrimo) proximoPrimo 
             where proximoPrimo = siguienteNumeroMayorN l n

criba :: Integer -> Set Integer
criba n = cribaAux [2..n] 1

---------------- ESTO NO ES PARTE DEL TP SOLO TESTEO----------------------
{-
menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | n == k = n
                      | n `mod` k == 0 = k
                      | otherwise = menorDivisorDesde n (k+1)

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = n == menorDivisor n
sonTODOSPRIMOS :: Set Integer -> Bool
sonTODOSPRIMOS [] = True
sonTODOSPRIMOS (x:xs) | not (esPrimo x) = False
                      | otherwise = sonTODOSPRIMOS xs
-}
-----------------------------------------------------------------------
--(3)
mcd :: Integer -> Integer -> Integer
mcd a 0 = abs a
mcd 0 b = abs b
mcd a b | (a == 1) || (b == 1) = 1
        | abs a > abs b = mcd (a `mod` b) b
        | abs b > abs a = mcd (b `mod` a) a

coprimoCon:: Integer -> Integer
coprimoCon n = coprimoConAux n 2

coprimoConAux :: Integer -> Integer -> Integer
coprimoConAux n i | sonCoprimos n i = i
                  | otherwise = coprimoConAux n (i+1)

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = mcd a b == 1


--(4)
inversoMultiplicativo:: Integer -> Integer -> Integer
inversoMultiplicativo n base = inversoMultiplicativoAux n 1 base

inversoMultiplicativoAux n k base | ((n*k) `mod` base) /= 1    = inversoMultiplicativoAux n (k+1) base
                                  | otherwise                  = k

-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1