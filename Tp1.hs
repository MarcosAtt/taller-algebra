module Tp1 where
-- Funciones del TP1, archivo de marcos

-- Funcion auxiliar
esUnCubo :: Integer -> Bool
esUnCubo x = (round (fromIntegral x**(1/3)))^3 == x

-- Funcion que indica si un numero es suma de dos cubos. N es tal que a^3 + b^3 = N
esSumaDeDosCubos :: Integer -> Bool
esSumaDeDosCubos n = esNSumaDeAHasta n 1 (mayorCuboAprox n)

-- Funcion auxiliar, Le resta el cubo de A a N hasta encontrar un cubo B.
esNSumaDeAHasta :: Integer -> Integer -> Integer -> Bool
esNSumaDeAHasta n a mayorCubo | esUnCubo b && b >= 1 = True
                              | a == mayorCubo       = False
                              | otherwise            = esNSumaDeAHasta n (a+1) mayorCubo
                                    where b = (n - a^3)

-- Devuelve el numero mas grande que al cubo es menor a N. Ej mayorCuboAprox 7 = 1, 1^3 <= 7.  mayorCuboAprox 8 = 2, 2^3 <= 8
mayorCuboAprox :: Integer -> Integer
mayorCuboAprox n = (floor (fromIntegral n**(1/3)))

-- Toma un entero y devuelve un par ordenado tal que a^3 + b^3 = n. Codigo de esNSumaDeAHasta reciclado.
descomposicionCubos :: Integer -> (Integer,Integer)
descomposicionCubos n = descomposicionCubosAux n 1 (mayorCuboAprox n)

descomposicionCubosAux :: Integer -> Integer -> Integer -> (Integer,Integer)
descomposicionCubosAux n a mayorCubo | not (esSumaDeDosCubos n) = (0,0)
                                     | esUnCubo b && b >= 1     = (a,(round (fromIntegral b**(1/3))))
                                     | otherwise                = descomposicionCubosAux n (a+1) mayorCubo
                                          where b = (n - a^3)

cantidadDeFormas :: Integer -> Integer
cantidadDeFormas n = 0

especialDesde :: Integer -> Integer
especialDesde n = 0

especialNumero :: Integer -> Integer
especialNumero n = 0

esMuyEspecial :: Integer -> Bool
esMuyEspecial n = False