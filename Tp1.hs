esSumaDeDosCubos :: Integer -> Bool

descomposicionCubos :: Integer -> (Integer,Integer)

cantidadDeFormas :: Integer -> Integer

especialDesde :: Integer -> Integer

especialNumero :: Integer -> Integer

esMuyEspecial :: Integer -> Bool

esUnCubo :: Integer -> Bool
esUnCubo x = (round (fromIntegral x**(1/3)))^3 == x
