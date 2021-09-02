# Clase 3 de haskell
* Recursion: Recursion.

* Implementar funcion de fibonacci.

* Implementar parteEntera: calcule la parte entera de un real positivo float>integer

1. Escribir una funcion para determinar si un numero natural es multiplo de 3. No esta
permitido utilizar mod ni div.
2. Implementar la funcion sumaImpares :: Int -> Int que dado n ∈N sume los primeros n
numeros impares. Ej: sumaImpares 3 > 1+3+5 > 9.
3. Escribir una funcion medioFact que dado n ∈ N calcula n!! = n (n −2)(n −4) ···. Por
ejemplo:
    * medioFact 10 > 10∗8∗6∗4∗2 > 3840.

    * medioFact 9 > 9∗7∗5∗3∗1 > 945.
4. Escribir una funcion que determine la suma de dıgitos de un numero positivo. Para esta funcion pueden utilizar div y mod.
5. Implementar una funcion que determine si todos los digitos de un numero son iguales.

cantidad de digitos: usar funciones aux

esimpar

cantidad de digitos impares

### Ej adicionales

* Implementar esPotencide2, dado un natural, determinar si es potencia de 2 , n = 2^k
* Implementar aQueExponente, regresa si existe, el exp k >= 0 en caso contrario devuelve -1.

* Implementar esAlternado dado un n entero n>=0 decide si no tiene dos digitos adyacentes con la misma paridad. alterna par e impar. Ej 12141983 = True , 0 = True, 1243 = False