{- HLINT ignore "Use even-}

f6 x y = x * x + y * y

g x y z = x + y + z * z

doble x = 2 * x

suma x y = x + y

normaVectorial x1 x2 = sqrt (x1 + x2)

funcionConstante8 x = 8

f n
  | n == 0 = 1
  | n /= 0 = 0

signo n
  | n > 0 = 1
  | n == 0 = 0
  | otherwise = -1

maximo2 x y
  | x >= y = x
  | otherwise = y

f1 n | n >= 3 = 5

f2 n
  | n >= 3 = 5
  | n <= 1 = 8

f3 n
  | n >= 3 = 5
  | n == 2 = undefined
  | otherwise = 8


f4 n
  | n >= 3 = 5
  | n <= 9 = 7

f5 n
  | n <= 9 = 7
  | n >= 3 = 5

esPar :: Int -> Bool
esPar n
  | mod n 2 == 0 = True
  | otherwise = False

esImpar :: Int -> Bool
esImpar n = not (esPar n)

funcionRara :: Float -> Float -> Bool -> Bool
funcionRara x y z = (x >= y) || z

funcionRara1 :: Float -> Float -> Bool -> Bool
funcionRara1 _ _ True = True
funcionRara1 x y False = x >= y

funcionRara2 :: Int -> Int -> Bool
funcionRara2 x y = x >= y

--Ejercicios Clase 1

absoluto :: Int -> Int
absoluto n
  | n < 0 = - n
  | n >= 0 = n

maximo :: Int -> Int -> Int
maximo x y
  | x >= y = x
  | otherwise = y

maximoAbsoluto :: Int -> Int -> Int 
maximoAbsoluto m n = maximo (absoluto m) (absoluto n)

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z  | x > y && x > z = x
               | y > x && y > z = y
               | z > x && z > y = z
               | otherwise = x

algunoEs0 :: Float -> Float -> Bool
algunoEs0 y x  = x == 0 || y == 0

ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y  = x == 0 && y == 0
               
algunoEs0p :: Float -> Float -> Bool
algunoEs0p 0 _ = True
algunoEs0p _ 0 = True
algunoEs0p _ _ = False             

ambosSon0p :: Float -> Float -> Bool
ambosSon0p 0 0 = True
ambosSon0p _ _ = False               


esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = mod x y == 0

digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

digitoDecenas :: Int -> Int 
digitoDecenas x = div (mod x 100) 10
