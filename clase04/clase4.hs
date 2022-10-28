{-# ANN module "HLint: ignore" #-}

sumatoria n | n == 0 = n
            | otherwise = n + sumatoria(n-1)

f1 :: Int -> Int
f1 n| n == 0 = 0  
    | otherwise = 2^n + f1(n-1)



f2 :: Int -> Float -> Float
f2 0 q = 0
f2 n q = q^n + f2(n-1) q

f3 :: Int -> Float -> Float
f3 0 q = 0
f3 n q = (f3(n-1) q) + q^(2*n-1) + q^(2*n)

f4 n q = f3 n q - (f2(n-1) q)

factorial :: Int -> Int
factorial n | n == 0 = 1
            | n > 0 = n * factorial (n-1)
    

eAprox :: Int -> Float
eAprox 0 = 1
eAprox n = (eAprox(n-1) + 1)/(fromIntegral(factorial n))

e :: Float
e = eAprox 10

f :: Int-> Int -> Int
f 0 m = 0
f n m = (f(n-1) m) + round (f2 m (fromIntegral n))

sumaPotencias :: Float -> Int -> Int -> Float
sumaPotencias q n 0 = 0
sumaPotencias q n m = (sumaPotencias q n (m-1)) + q^m*(f2 n q)

sumaRacionales :: Int -> Int -> Float
sumaRacionales n 0 = 0
sumaRacionales n m = (sumaRacionales n (m-1)) + (fromIntegral(sumatoria n))/(fromIntegral m)
            
