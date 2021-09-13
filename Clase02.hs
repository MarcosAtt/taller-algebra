module Clase02 where
import ModuloVectores
identidad :: t -> t
identidad x = x

primero :: tx -> ty -> tx
primero x y = x

segundo :: tx -> ty -> ty
segundo x y = y

constante5 :: tx -> ty -> tz -> Int
constante5 x y z = 5

mismoTipo :: t -> t -> Bool
mismoTipo x y = True

triple :: Num t => t -> t
triple x = 3 * x

maximo x y | x >= y = x
           | otherwise = y

distintos x y = x /= y

cinco :: Int
cinco = 5

f1 x y z = x ** y + z <= x+y ** z

f2 x y = ( sqrt x) / ( sqrt y)

f3 x y = div ( sqrt x) ( sqrt y)

f4 x y z | x == y = z
         | x ** y == y = z
         | otherwise = z

f5 x y z | x == y = z
         | x ** y == y = x
         | otherwise = y

suma (vx, vy) (wx, wy) = (vx + wx, vy + wy)

factorial 0 = 1
factorial x = factorial (x - 1) * x

fib 0 = 1
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

esOrigen :: (Float, Float) -> Bool
esOrigen (0,0) = True
esOrigen (_,_) = False

angulo0 :: (Float, Float) -> Bool
angulo0 (_, 0) = True
angulo0 (_, _) = False

angulo45 :: (Float, Float) -> Bool
angulo45 (x, y) = x == y

patterMatching :: (Float, (Bool, Int), (Bool, (Int, Float))) -> (Float, (Int, Float))
patterMatching (f1, (True, _), (_, (0, f2))) = (f1, (1, f2))
patterMatching (_,_, (_, (_, f1))) = (f1, (0, f1))

-- 1 estanRelacionados: dados dos numeros reales, decide si estan relacionados considerando
-- la relacion de equivalencia en R cuyas clases de equivalencia son: (−∞, 3], (3, 7] y (7, ∞).
estanRelacionados :: Float -> Float -> Bool
estanRelacionados x y | (x <= 3) && (y <= 3) = True
                      | ((x > 3) && (x <= 7)) && ((y > 3) && (y <= 7)) = True
                      | (x > 7) && (y > 7) = True
                      | otherwise = False

--2 prodInt: calcula el producto interno entre dos vectores de R2.
prodIntR2 :: (Float, Float) -> (Float, Float) -> Float
prodIntR2 (vx, vy) (wx, wy) = vx*wx + vy*wy

--3 todoMenor: dados dos vectores de R2, decide si es cierto que cada coordenada del primer
--vector es menor a la coordenada correspondiente del segundo vector.
todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (vx, vy) (wx, wy) = (vx < wx) && (vy < wy)

--4 distanciaPuntos: calcula la distancia entre dos puntos de R2.
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (vx, vy) (wx, wy) = sqrt((wx-vx)^2 + (wy-vy)^2)

--5 sumaTerna: dada una terna de enteros, calcula la suma de sus tres elementos.
sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (x, y, z) = x+y+z

--6 posicPrimerPar: dada una terna de enteros, devuelve la posicion del primer numero par si
--es que hay alguno, y devuelve 4 si son todos impares.
posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (a, b, c) | mod a 2 == 0 = 1
                         | mod b 2 == 0 = 2
                         | mod c 2 == 0 = 3
                         | otherwise = 4

--7 crearPar :: a -> b -> (a, b): crea un par a partir de sus dos componentes dadas por
--separado (debe funcionar para elementos de cualquier tipo).
crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)
--8 invertir :: (a, b) -> (b, a): invierte los elementos del par pasado como par´ametro
--(debe funcionar para elementos de cualquier tipo).
invertir :: (a, b) -> (b, a)
invertir (a, b) = (b, a)

prodVecR3 :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
prodVecR3 (a1,a2,a3) (b1,b2,b3) = ((a2*b3-a3*b2),(-(a1*b3-a3*b1)),((a1*b2-a2*b1)))
{-
determinanteR3 :: (Int, Int, Int) -> (Int, Int, Int) -> Int
determinanteR3 (a1,a2,a3) (b1,b2,b3) = 1 -}

division :: (Integral t) => t -> t -> (t, t)
division n k = ((div n k), (mod n k))

multiplicacionCompleja :: (Num t) => (t, t) -> (t, t) -> (t, t)
multiplicacionCompleja (a,b) (c,d) = ((a*c - b*d), (a*d + b*c))


cosVectores :: (Floating t) => (t,t,t) -> (t,t,t) -> t 
cosVectores (a,b,c) (x,y,z) = (productoInterno primerV segundoV) / ((norma primerV)*(norma segundoV))
                            where
                                primerV = (a,b,c)
                                segundoV = (x,y,z)

-- |Dados dos vectores en R3, devuelve su producto vectorial.
productoVectorial :: (Floating t) => (t,t,t) -> (t,t,t) -> (t,t,t)
productoVectorial (a1,a2,a3) (b1,b2,b3) = ((a2*b3-a3*b2),(-(a1*b3-a3*b1)),((a1*b2-a2*b1)))
