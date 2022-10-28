comb :: Int -> Int -> Int
comb n m    | n < m = 0
            | m == 0 = 1
            | otherwise = (comb (n-1) m)  + (comb(n-1) (m-1))

{-faltan correciones-}
variaciones :: Set Int -> Int -> Set [Int]
variaciones xs m = agregarLLC(variaciones xs m-1) xs

agregarLLC yss [] = []
agregarLLC yss (x:xs) = (agregarLLC yss xs) ++ agregarLLE yss x

agregarLLE [] x = []
agregarLLE (ys:yss) x = [ys ++ [x]] ++ agregarLLE yss x