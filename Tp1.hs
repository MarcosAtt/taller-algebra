module Tp1 where
-- Funciones del TP1, archivo de marcos

-- Funcion auxiliar
esUnCubo :: Integer -> Bool
esUnCubo x = round (fromIntegral x**(1/3))^3 == x

raizCubica :: Integer -> Integer
raizCubica x = round (fromIntegral x**(1/3))

-- 1 - Funcion que indica si un numero es suma de dos cubos. N es tal que a^3 + b^3 = N,  con N perteneciente a los numeros Naturales.
esSumaDeDosCubos :: Integer -> Bool
esSumaDeDosCubos n = n >= 1 && esNSumaDeAHasta n 1 (mayorCuboAprox n)

-- Funcion auxiliar, Le resta el cubo de A a N hasta encontrar un cubo B.
esNSumaDeAHasta :: Integer -> Integer -> Integer -> Bool
esNSumaDeAHasta n a mayorCubo | esUnCubo b && b >= 1 = True
                              | a == mayorCubo       = False
                              | otherwise            = esNSumaDeAHasta n (a+1) mayorCubo
                                    where b = n - a^3

-- Devuelve el numero mas grande que al cubo es menor a N. Ej mayorCuboAprox 7 = 1, 1^3 <= 7.  mayorCuboAprox 8 = 2, 2^3 <= 8
mayorCuboAprox :: Integer -> Integer
mayorCuboAprox n = floor (fromIntegral n**(1/3))

-- 2 Toma un entero y devuelve un par ordenado tal que a^3 + b^3 = n. Codigo de esNSumaDeAHasta reciclado.
descomposicionCubos :: Integer -> (Integer,Integer)
descomposicionCubos n | not (esSumaDeDosCubos n) = (0,0)
                      | otherwise = descomposicionCubosAux n 1 (mayorCuboAprox n)

descomposicionCubosAux :: Integer -> Integer -> Integer -> (Integer,Integer)
descomposicionCubosAux n a mayorCubo | esUnCubo b && b >= 1     = (a, raizCubica b)
                                     | otherwise                = descomposicionCubosAux n (a+1) mayorCubo
                                          where b = n - a^3

-- 3 Devuelve la cantidad de formas de escribir
cantidadDeFormas :: Integer -> Integer
cantidadDeFormas n | not (esSumaDeDosCubos n) = 0
                   | otherwise                = cantidadDeFormasAux n 1 (mayorCuboAprox n) `div` 2 -- lo divido por 2 porque repite cada caso exactamente 2 veces.

cantidadDeFormasAux :: Integer -> Integer -> Integer -> Integer
cantidadDeFormasAux n a cota | a > cota             = 0
                             | esUnCubo b && b >= 1 = 1 + cantidadDeFormasAux n (a+1) cota
                             | otherwise            =     cantidadDeFormasAux n (a+1) cota
                                    where b = n - a^3

-- 4 - Siguiente numero especial desde n. (especial: cantidad de formas >= 2)
especialDesde :: Integer -> Integer
especialDesde n | n <= 1729 = 1729
                | cantidadDeFormas n >= 2 = n
                | otherwise = especialDesde (n+1)

-- 5 - N-esimo especial.
especialNumero :: Integer -> Integer
especialNumero n = especialNumeroAux 1729 n

especialNumeroAux :: Integer -> Integer -> Integer
especialNumeroAux n c | c == 1 = n
                      | otherwise = especialNumeroAux (especialDesde (n+1)) (c-1)

-- 6 - Devuelve True si un numero especial no se puede escribir como producto de un cubo por otro especial.
esMuyEspecial :: Integer -> Bool
esMuyEspecial n | (cantidadDeFormas n) < 2 = False
                | otherwise = esMuyEspecialAux n 2 1729

-- Funcion auxiliar que va a calcular si el numero n es multiplo de algun k^3 * m , siendo m un numero especial anterior a n.
esMuyEspecialAux :: Integer -> Integer -> Integer -> Bool
esMuyEspecialAux n k m | n == (k^3) * m = False
                       | n >  (k^3) * m = esMuyEspecialAux n (k+1) m 
                       | m >= n = True
                       | otherwise = esMuyEspecialAux n 2 (especialDesde (m+1)) 