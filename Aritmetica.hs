module Aritmetica where
import Tipos
import Data.Tuple
import Data.Bits

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
-- Ejercicio 1: Lista de todos los primos hasta n.
criba :: Integer -> Set Integer
criba n = cribaAux [2..n] 1

mcd :: Integer -> Integer -> Integer
mcd a 0 = abs a
mcd 0 b = abs b
mcd a b | (a == 1) || (b == 1) = 1
        | abs a >= abs b = mcd (a `mod` b) b
        | otherwise = mcd (b `mod` a) a

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = mcd a b == 1

coprimoConAux :: Integer -> Integer -> Integer
coprimoConAux n i | sonCoprimos n i = i
                  | otherwise = coprimoConAux n (i+1)
-- Ejercicio 2: Encuentra un numero coprimo k con n (n:k) = 1
coprimoCon:: Integer -> Integer
coprimoCon n = coprimoConAux n 2

emcd :: Integer -> Integer -> (Integer, Integer, Integer)
emcd 0 b = (b, 0, 1)
emcd a b = (d, t - (b `div` a) * s, s)
        where (d, s, t) = emcd (b `mod` a) a

-- Ejercicio 3: Inverso multiplicativo n m = i / n*i (mod m) = 1 .
{- Sacar el inverso multiplicativo es como resolver la ecuacion de congruencia n =(cong)= 1 (mod m)
 y con el emcd tengo una solucion de lo puede valer s tal que s*n - t*m = 1 ya que n y m son coprimos. 
 Entonces s (m) = inversoMult -}
inversoMultiplicativo:: Integer -> Integer -> Integer
inversoMultiplicativo n m | sonCoprimos n m = s `mod` m
                          | otherwise = 0
        where (d, s, t) = emcd n m

-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1