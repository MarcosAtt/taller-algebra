module RSA where
import Tipos
import Aritmetica

-- Ejercicio 4: Dados dos primos p q, genera claves posibles.
claves :: Integer -> Integer -> (Integer, Integer, Integer)
claves p q = (e, d, n)
  where n = p*q
        e = coprimoCon m -- 1<e<m, e coprimo m
        d = inversoMultiplicativo e m
        m = (p-1)*(q-1)

-- Ejercicio 5: Dada una clave publica (e, n) y un mensaje (lista de Chars) devuelve el mensaje encriptado (lista de Integers).
codificador :: Clpub -> Mensaje -> Cifrado
codificador clavePublica mensaje = codificarLetras clavePublica (aEnteros mensaje)

codificarLetras :: Clpub -> Cifrado -> Cifrado
codificarLetras clavePublica [] = []
codificarLetras (e, n) (a:as) = (modExp a e n):(codificarLetras (e,n) as)

-- Ejercicio 6: Dada una clave privada (d, n) y un mensaje encriptado (lista de Integers), devuelve el mensaje original.
decodificador :: Clpri -> Cifrado -> Mensaje
decodificador clavePrivada mensajeCifrado = aChars (decodificarLetras clavePrivada mensajeCifrado)

decodificarLetras :: Clpri -> Cifrado -> Cifrado
decodificarLetras clavePrivada [] = []
decodificarLetras (d, n) (a:as) = (modExp a d n):(decodificarLetras (d,n) as)