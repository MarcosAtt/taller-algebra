module Clase11 where
-- Clase de numeros complejos

type Complejo = (Float, Float) -- z = a + bi
type Polar = (Float, Float) -- z = (r, angulo)
-- warning funcion argumento de polares esta mal implementada

re :: Complejo -> Float
re z = fst z

im :: Complejo -> Float
im z = snd z

conjugado :: Complejo -> Complejo
conjugado (a, b) = (a,-b)

suma :: Complejo -> Complejo -> Complejo
suma (a,b) (c,d) = (a+c, b+d)

producto :: Complejo -> Complejo -> Complejo
producto (a,b) (c,d) = (a*c-b*d, a*d+b*c)

inverso :: Complejo -> Complejo
inverso (a,b) = (a/m, -b/m)
    where m = a**2+b**2



cociente :: Complejo -> Complejo -> Complejo
cociente z (0,0) = undefined
cociente (a,b) (c,d) = ((a*c+b*d)/divisor, (b*c-a*d)/divisor)
    where divisor = c^2 + d^2


--potencia :: Complejo -> Int -> Complejo
--solucionesCuadratica :: Float -> Float -> Float -> (Complejo, Complejo)

modulo :: Complejo -> Float
modulo (a,b) = sqrt (a^2+b^2)

argumento :: Complejo -> Float
argumento (a,b) = atan(b/a)

pasarACartesianas :: Float -> Float -> Complejo
pasarACartesianas r tita = (r*cos(tita), r*sin(tita))
--raizCuadrada :: Complejo -> (Complejo, Complejo)

--solucionesCuadraticaCoefComplejos :: Complejo -> Complejo -> Complejo (Complejo, Complejo)

--raicesNEsimas :: Int -> [Complejo]

--potenciasRaizNEsima :: Int -> Int -> [Complejo]