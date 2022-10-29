
factorial :: Int -> Int
factorial n | n == 0 = 1
            | n > 0 = n * factorial (n-1)
    
        
esPar :: Int -> Bool
esPar n | n == 0 = True
        | n == 1 = False
        | otherwise = esPar (n-2)

-- Ejercicios clase 3

-- 1) implementar una funcion fib que devuelva el enesimo numero de Fibonacci
fib :: Int -> Int
fib n   | n == 0 = 0
        | n == 1 = 1
        | otherwise = fib(n-1) + fib(n-2)

-- 2) Implementar una funcion parteEntera :: Float -> Integer que calcule
-- la parte entera de un numero real positivo
parteEntera :: Float -> Int
parteEntera x   | x < 1 = 0
                | x >= 1 = 1 + parteEntera(x-1)

-- Escribir la funcion para determinar si un numero natural es multiplo
-- de 3. No esta permitido utilizar mod ni div

esMultiploDe3 :: Int -> Bool
esMultiploDe3 n | n == 1 = False
                | n == 2 = False
                | n == 3 = True
                | n > 2 = esMultiploDe3(n-3)


-- Implementar la funcion sumaImpares :: Int -> Int que dado n ∈ N sume los primeros n
-- numeros impares. Ej: sumaImpares 3 1+3+5 9.
nEsimoImpar :: Int -> Int
nEsimoImpar n = n*2 - 1

sumaImpares :: Int -> Int
sumaImpares n   | n == 0 = 0
                | n < 0 = 0
                | n > 0 = nEsimoImpar n + sumaImpares(n-1)

--Escribir una funcion medioFact que dado n ∈ N calcula n!! = n (n − 2)(n − 4) · · · . Por
--ejemplo:
--medioFact 10 10 ∗ 8 ∗ 6 ∗ 4 ∗ 2 3840.
--medioFact 9 9 ∗ 7 ∗ 5 ∗ 3 ∗ 1 945

medioFact :: Int -> Int
medioFact n | n == 0 = 1
            | n == 1 = 1
            | n > 1 = n* medioFact(n-2)

-- Escribir una funcion que determine la suma de dıgitos de un numero positivo. Para esta
-- funcion pueden utilizar div y mod.

unidad :: Int -> Int
unidad n = mod n 10

-- unidad 34545616 = 3
-- div 34545616 = 3454561

sumaDigitos :: Int -> Int
sumaDigitos n   | n < 10 = n
                | n >= 10 = unidad n + sumaDigitos(div n 10)

-- Implementar una funcion que determine si todos los dıgitos de un numero son iguales.                

decena :: Int -> Int
decena n = unidad(div n 10)

digitosIguales :: Int -> Bool
digitosIguales n    | n < 10 = True
                    | n >= 10 = unidad n == decena n && digitosIguales(div n 10)

                    