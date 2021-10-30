module RSA where
import Tipos
import Aritmetica


--(3)
claves :: Integer -> Integer -> (Integer, Integer, Integer)
claves p q = (e, d, n)
  where n = p*q
        e = coprimoCon cota -- 1<e<cota, e coprimo cota
        d = inversoMultiplicativo e cota -- d inverso multiplicativo e (cota)
        cota = (p-1)*(q-1)

--(6)
codificador :: Clpub -> Mensaje -> Cifrado
codificador clave m = codificadorAux clave (aEnteros m)

codificadorAux :: Clpub -> Cifrado -> Cifrado
codificadorAux clave [] = []
codificadorAux (d, n) (a:as) = (modExp a d n):(codificadorAux (d,n) as)

--(7)
decodificador :: Clpri -> Cifrado -> Mensaje
decodificador clave m = aChars (decodificadorAux clave m)

decodificadorAux :: Clpub -> Cifrado -> Cifrado
decodificadorAux clave [] = []
decodificadorAux (e, n) (a:as) = (modExp a e n):(decodificadorAux (e,n) as)