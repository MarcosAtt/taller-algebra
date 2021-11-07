module Aritmetica where
import Tipos
import Data.Tuple
import Data.Bits

siguienteNumeroMayorN :: Set Integer -> Integer -> Integer
siguienteNumeroMayorN [] n = n
siguienteNumeroMayorN (x:xs) n | x > n = x
                               | otherwise = siguienteNumeroMayorN xs n

eliminarMultiplos :: Set Integer -> Integer -> Set Integer
eliminarMultiplos [] _ = []
eliminarMultiplos (x:xs) n | (x `mod` n == 0) && (x > n) = eliminarMultiplos xs n
                           | otherwise                   = x:(eliminarMultiplos xs n)

eliminarCompuestos :: Set Integer -> Integer -> Set Integer
eliminarCompuestos l n | proximoPrimo == n = l
                       | otherwise = eliminarCompuestos (eliminarMultiplos l proximoPrimo) proximoPrimo 
        where proximoPrimo = siguienteNumeroMayorN l n

-- Ejercicio 1: Lista de todos los primos hasta n.
criba :: Integer -> Set Integer
criba n = eliminarCompuestos [2..n] 1

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = mcd == 1
        where (mcd, s, t) = emcd a b

coprimoConAux :: Integer -> Integer -> Integer
coprimoConAux n k | sonCoprimos n k = k
                  | otherwise = coprimoConAux n (k+1)

-- Ejercicio 2: Encuentra un numero coprimo k con n, es decir (n:k) = 1
coprimoCon :: Integer -> Integer
coprimoCon n = coprimoConAux n 2

-- Algoritmo extendido de Euclides. Devuelve (mcd, s, t)
emcd :: Integer -> Integer -> (Integer, Integer, Integer)
emcd 0 b = (b, 0, 1)
emcd a b = (d, t - (b `div` a) * s, s)
        where (d, s, t) = emcd (b `mod` a) a

-- Ejercicio 3: Inverso multiplicativo n m = i / n*i (mod m) = 1
{- Sacar el inverso multiplicativo es como resolver la ecuacion de congruencia n =(cong)= 1 (mod m)
 y con el emcd tengo una solucion de lo que puede valer s tal que s*n - t*m = 1 ya que n y m son coprimos. 
 Entonces s (m) = inversoMult -}
inversoMultiplicativo :: Integer -> Integer -> Integer
inversoMultiplicativo n m | sonCoprimos n m = s `mod` m
                          | otherwise = 0
        where (d, s, t) = emcd n m

-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1