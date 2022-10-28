

--sumatoria :: [Int] -> Int
--sumatoria [] = 0
--sumatoria xs = head xs + sumatoria (tail + xs)

--longitud :: [Int] -> Int
--longitud xs = 1 + longitud(tail xs)

--pertenece :: Int -> [Int] -> Bool
--pertenece x [] = False
--pertenece x xs = x == (head xs) || pertenece x (tail xs)


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
sumarN n (x:xs) = (n+x) : (sumarN n xs)










