module Tp1 where

-- Funcion auxiliar
esUnCubo :: Integer -> Bool
esUnCubo x = (round (fromIntegral x**(1/3)))^3 == x

-- Funcion que indica si un numero es suma de dos cubos. N es a^3 + b^3 = N ?
esSumaDeDosCubos :: Integer -> Bool
esSumaDeDosCubos n = False




descomposicionCubos :: Integer -> (Integer,Integer)
descomposicionCubos n = (0,0)

cantidadDeFormas :: Integer -> Integer
cantidadDeFormas n = 0

especialDesde :: Integer -> Integer
especialDesde n = 0

especialNumero :: Integer -> Integer
especialNumero n = 0

esMuyEspecial :: Integer -> Bool
esMuyEspecial n = False