module Clase6 where

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria xs = head xs + sumatoria (tail xs)


longitud :: [Int] -> Int
longitud [] = 0
longitud xs = 1 + longitud(tail xs)

pertenece :: Int -> [Int] -> Bool
pertenece x [] = False
pertenece x xs = x == (head xs) || pertenece x (tail xs)


-- usando pattern matching

--pertenece :: Int -> [Int] -> Bool
--pertenece t [] = False
--pertenece t (x:xs) = t  == (head xs) || pertenece t (tail xs)

-- ejercicios
{- HLINT ignore -}


productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * (productoria xs)





sumarN :: Int -> [Int] -> [Int]
sumarN n [] = []
sumarN n (x:xs) = ((n+x) : (sumarN n xs))

sumatoria':: [Int] -> Int
sumatoria' [] = 0
sumatoria' (x:xs) = x + sumatoria' xs

sumarElPrimero:: [Int] -> [Int]
sumarElPrimero xxs  | xxs == [] = []
                    | otherwise = sumarN (head xxs) (xxs)
                             
sumarElPrimero' :: [Int] -> [Int]
sumarElPrimero' ( x : xs ) = sumarN x ( x : xs )

capturarUltimo:: [Int] -> Int
capturarUltimo xs   | xs == [] = 0
                    | longitud xs == 1 = head xs
                    | otherwise = capturarUltimo (tail xs)


sumarElUltimo:: [Int] -> [Int]
sumarElUltimo xxs   | xxs == [] = []
                    | otherwise = sumarN(capturarUltimo xxs) xxs

sumarElUltimo':: [Int] -> [Int]
sumarElUltimo'(x:xs) = (sumarN(capturarUltimo  (x:xs)) (x:xs))

esPar:: Int -> Bool
esPar x | mod x 2 == 0 = True
        | otherwise = False

pares:: [Int] -> [Int]
pares xs    | xs == [] = []
            | esPar (head xs) == True = (head xs):pares (tail xs) 
            | esPar (head xs) == False = pares (tail xs)

pares':: [Int] -> [Int]
pares' [] = []
pares' (x:xs)   | esPar x == True =  x : pares'(x:xs)
                | esPar x == False = pares (x:xs)

esMultiplo:: Int -> Int -> Bool
esMultiplo x y  | mod x y == 0 = True
                | otherwise = False

multiplosDeN:: Int -> [Int] -> [Int]
multiplosDeN n xs   | xs == [] = []
                    | esMultiplo (head xs) n  = head xs : multiplosDeN n (tail xs)
                    | not(esMultiplo (head xs) n ) = multiplosDeN n (tail xs)

multiplosDeN':: Int -> [Int] -> [Int]
multiplosDeN' n [] = []
multiplosDeN' n (x:xs)  | esMultiplo x n = x : multiplosDeN' n xs
                        | not(esMultiplo x n) = multiplosDeN' n xs

reverso:: [Int] -> [Int]
reverso xs  | xs == [] = []
            | otherwise = capturarUltimo xs : reverso (tail xs)


                   







