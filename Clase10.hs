module Clase10 where
import Clase05
import Clase09
{-Sistemas de ecuaciones lineales de congruencia-}

type Ecuacion = (Int, Int, Int)--Ecuacion aX _=_ b (m) ---> (a,b,m)
type Solucion = (Int, Int)     --Solucion X_=_ r (m) ---> (r,m)
type SistemaEc = [Ecuacion]
type SistemaSimplifEc = [Solucion]
-- Simplifico el sistema
ecEquivalente :: Ecuacion -> Ecuacion
ecEquivalente (a,b,m) | (b `mod` d == 0) = (a`div`d, b`div`d, m`div`d)
                      | otherwise = undefined
                      where d = mcd a m

solucionEcConPropAdic :: Ecuacion -> Solucion
solucionEcConPropAdic (a, b, m) = ((s*b) `mod` m, m)
    where (d, s, t) = emcd a m

solucionEc :: Ecuacion -> Solucion
solucionEc e = solucionEcConPropAdic (ecEquivalente e)

sistemaSimplifEquiv :: SistemaEc -> SistemaSimplifEc
sistemaSimplifEquiv [] = []
sistemaSimplifEquiv (e:es) = (solucionEc e):(sistemaSimplifEquiv es)
-- Busco todos los primos que aparecen repetidos en el sistema
modulos :: SistemaSimplifEc -> [Int]
modulos [] = []
modulos ((r,m):es) = m:(modulos es)

mayorModulo :: SistemaSimplifEc -> Int
mayorModulo sistema = maximum (modulos sistema)

cotaParaPrimoMaloDesde :: SistemaSimplifEc -> Int -> Int
cotaParaPrimoMaloDesde sistema n | nEsimoPrimo (n+1) > (mayorModulo sistema) = n
                                 | otherwise = cotaParaPrimoMaloDesde sistema (n+1)

cotaParaPrimoMalo :: SistemaSimplifEc -> Int
cotaParaPrimoMalo sistema = cotaParaPrimoMaloDesde sistema 1

cantidadMultiplos :: [Int] -> Int -> Int
cantidadMultiplos [] _ = 0
cantidadMultiplos (m:ms) n | m `mod` (nEsimoPrimo n) == 0 = 1 + cantidadMultiplos ms n
                           | otherwise = cantidadMultiplos ms n

esPrimoMalo :: SistemaSimplifEc -> Int -> Bool
esPrimoMalo sistema n = cantidadMultiplos (modulos sistema) n >= 2

todosLosPrimosMalosHasta :: SistemaSimplifEc -> Int -> [Int]
todosLosPrimosMalosHasta _ 0 = []
todosLosPrimosMalosHasta sistema n | esPrimoMalo sistema n = (nEsimoPrimo n):(todosLosPrimosMalosHasta sistema (n-1))
                              | otherwise = todosLosPrimosMalosHasta sistema (n-1)

todosLosPrimosMalos :: SistemaSimplifEc -> [Int]
todosLosPrimosMalos [] = []
todosLosPrimosMalos sistema = todosLosPrimosMalosHasta sistema (cotaParaPrimoMalo sistema)

solucDosEcPotenciasPrimosOrd :: Solucion -> Solucion -> Solucion
solucDosEcPotenciasPrimosOrd (r1, m1) (r2, m2) | (r2-r1) `mod` m1 == 0 = (r2, m2)
                                               | otherwise = undefined

solucDosEcPotenciasPrimo :: Solucion -> Solucion -> Solucion
solucDosEcPotenciasPrimo (r1, m1) (r2, m2) | m1 <= m2 = solucDosEcPotenciasPrimosOrd (r1, m1) (r2, m2)
                                           | otherwise = solucDosEcPotenciasPrimosOrd (r2, m2) (r1, m1)

solucSistemaPotenciasPrimo :: SistemaSimplifEc -> Solucion
solucSistemaPotenciasPrimo [e] = e
solucSistemaPotenciasPrimo (e1:e2:es) = solucSistemaPotenciasPrimo ((solucDosEcPotenciasPrimo e1 e2):es)

--dados m y p calcula la mayor potencia de p que divide a m
quePotenciaLoDivide :: Int -> Int -> Int
quePotenciaLoDivide m p = quePotenciaLoDivideAux m p 0

quePotenciaLoDivideAux :: Int -> Int -> Int -> Int
quePotenciaLoDivideAux m p c | m `mod` p^c == 0 = quePotenciaLoDivideAux m p (c+1)
                             | otherwise        = c-1

desdoblarSistemaEnFcionPrimo :: SistemaSimplifEc -> Int -> (SistemaSimplifEc, SistemaSimplifEc)
desdoblarSistemaEnFcionPrimo [] _ = ([], [])
desdoblarSistemaEnFcionPrimo ((r, m):es) p | k == 0 = (pri, (r, m):seg)
                                           | m == p^k = ((r, m):pri, seg)
                                           | otherwise = ((mod r (p^k), p^k):pri, (mod r (div m (p^k)), div m (p^k)):seg)
 where (pri, seg) = desdoblarSistemaEnFcionPrimo es p
       k = quePotenciaLoDivide m p


sistemaEquivSinPrimosMalosAux :: SistemaSimplifEc -> [Int] -> SistemaSimplifEc
sistemaEquivSinPrimosMalosAux sistema [] = sistema
sistemaEquivSinPrimosMalosAux sistema (p:ps) = (solucSistemaPotenciasPrimo pri):(sistemaEquivSinPrimosMalosAux seg ps)
    where (pri, seg) = desdoblarSistemaEnFcionPrimo sistema p

sistemaEquivSinPrimosMalos :: SistemaSimplifEc -> SistemaSimplifEc
sistemaEquivSinPrimosMalos sistema = sistemaEquivSinPrimosMalosAux sistema (todosLosPrimosMalos sistema)

solucSistemaModCoprimos :: SistemaSimplifEc -> Solucion
solucSistemaModCoprimos [e] = e
solucSistemaModCoprimos ((r1, m1):(r2, m2):es) = solucSistemaModCoprimos ((r, m1*m2):es)
    where (d, s, t) = emcd m1 m2
          r = mod (r1*t*m2 + r2*s*m1) (m1*m2)
-- godines
solucSistema :: SistemaEc -> Solucion
solucSistema sistema = solucSistemaModCoprimos (sistemaEquivSinPrimosMalos (sistemaSimplifEquiv sistema))