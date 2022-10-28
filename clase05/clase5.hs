-- me creo otra funcion para poder resolver sumaDivisores, (creo otro parametro m para poder iterar)
--                    n      m
{- HLINT ignore -}

sumaDivisoresHasta :: Int-> Int -> Int
sumaDivisoresHasta n 0 = 0
sumaDivisoresHasta n m  | esDivisor m n = m + sumaDivisoresHasta n (m-1)
                        | not(esDivisor m n) = sumaDivisoresHasta n (m-1)


esDivisor :: Int -> Int -> Bool
esDivisor m n = mod n m == 0


sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n

menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n m   | esDivisor m n = m
                        | not(esDivisor m n) = menorDivisorDesde n (m+1)

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = (menorDivisor n) == n


nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = menorPrimoDesde(1 + nEsimoPrimo(n-1))


menorPrimoDesde :: Int -> Int
menorPrimoDesde n   | (esPrimo n) = n 
                    | otherwise = menorPrimoDesde(n+1)

factorial :: Integer -> Integer
factorial n | n == 0 = 1
            | n > 0 = n * factorial (n-1)


menorFactDesdeDesde:: Integer -> Integer -> Integer
menorFactDesdeDesde k m | (factorial k) >= m = factorial k
                        | otherwise = menorFactDesdeDesde (k+1) m

menorFactDesde :: Integer -> Integer
menorFactDesde m = menorFactDesdeDesde 1 m

mayorFactHastaHasta :: Integer -> Integer -> Integer
mayorFactHastaHasta k m | (factorial k) <= m = factorial k
                        | otherwise = mayorFactHastaHasta (k-1) m

mayorFactHasta :: Integer -> Integer
mayorFactHasta m = mayorFactHastaHasta m m       

-- otra solucion

--mayorFactHastaDesde :: Integer -> Integer -> Integer
--mayorFactHastaDesde k m | (factorial k) > m = factorial (k-1)
--                        | (factorial k) <= m = mayorFactHastaDesde (k+1)

--mayorFactHasta :: Integer -> Integer
--mayorFactHasta m = mayorFactHastaHasta 1 m   




                    