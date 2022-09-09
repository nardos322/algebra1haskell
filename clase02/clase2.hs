
indentidad :: t -> t
indentidad x = x

primero :: tx -> ty -> tx
primero x y = x

triple :: (Num t) => t -> t
triple x = 3*x

maximo :: (Ord t) => t -> t -> t
maximo x y  | x >= y = x
            | otherwise = y

distintos :: (Eq t) => t -> t -> Bool
distintos x y = x /= y

cantidadDeSoluciones :: (Num t, Ord t) => t -> t -> Int
cantidadDeSoluciones b c    | d > 0 = 2
                            | d == 0 = 1
                            | otherwise = 0
                            where d = b^2 - 4*c 

pepe :: (Floating t, Eq t, Num u, Eq u) => t ->  t -> u -> Bool -- la variable t tiene que ser de un tipo
pepe x y z = sqrt (x + y) == x && 3*z == 0                      -- que pertenezca a Floating y Eq
                                                                -- la variable u tien que ser de un tipo que prertenezca a Num y Eq

f1 x y z = x ** y + z <= x+y ** z
f2 x y = ( sqrt x) / ( sqrt y)
f3 x y = div ( sqrt x) ( sqrt y)
f4 x y z    | x == y = z
            | x ** y == y = x
            | otherwise = y

f5 x y z    | x == y = z
            | x ** y == y = z
            | otherwise = z
                                                           