{-HLINT ignore -}

identidad :: t -> t
identidad x = x

primero :: tx -> ty -> tx
primero x y = x

segundo :: tx -> ty -> ty
segundo x y = y

constante5 :: p1 -> p2 -> p3 -> Float
constante5 x y z = 5.0

mismoTipo :: t -> t -> Bool
mismoTipo x y = True

triple :: (Num t) => t -> t
triple x = 3*x

maximo :: (Ord t) => t -> t -> t
maximo x y  | x >= y = x
            | otherwise = y

cinco :: Floating t => t
cinco = 5            

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
            | x ** y == y = z
            | otherwise = z

f5 x y z    | x == y = z
            | x ** y == y = x
            | otherwise = y
p2 x = x + x  

tupla :: (Int, Int) 
tupla = (1, 2)
 

suma :: (Float , Float ) -> (Float , Float ) -> (Float , Float )
suma v w = (( fst v) + ( fst w ) , ( snd v) + ( snd w))

esOrigen :: (Ord t, Num t) => (t, t) -> Bool
esOrigen (0 , 0) = True
esOrigen (_ , _) = False

angulo0 :: (Float , Float ) -> Bool
angulo0 (_ , 0) = True
angulo0 (_ , _ ) = False

angulo45 :: (Float , Float ) -> Bool
angulo45 (x , y) = x == y

patternMatching :: (Float , (Bool , Int ) , (Bool , (Int , Float )) ) -> (Float , (Int , Float ))
patternMatching ( f1 , (True , _ ) , (_ , (0 , f2 ) )) = (f1 , (1 , f2 ))
patternMatching ( _ , _ , (_ , (_ , f ))) = (f , (0 , f))

-- | normaVectorial2 x y es la norma de (x,y)
normaVectorial2 :: Float -> Float -> Float
normaVectorial2 x y = sqrt (x ^2 + y ^2)
-- | normaVectorial1 (x,y) es la norma de (x,y)
normaVectorial1 :: (Float , Float ) -> Float
normaVectorial1 (x ,y) = sqrt (x ^2 + y ^2)

norma1Suma :: (Float , Float ) -> (Float , Float ) -> Float
norma1Suma v1 v2 = normaVectorial1 ( suma v1 v2 )

norma2Suma :: (Float , Float ) -> (Float , Float ) -> Float
norma2Suma v1 v2 = normaVectorial2 (fst s) ( snd s) where s = suma v1 v2

{-
estanRelacionados: dados dos numeros reales, decide si estan relacionados considerando
la relacion de equivalencia en R cuyas clases de equivalencia son:
(−∞, 3], (3, 7] y (7, ∞)

-}

estanRelacionados :: Int -> Int -> Bool
estanRelacionados x y   = ( x <= 3 && y <= 3) || ( x > 3 && x <= 7 && y > 3 && y <= 7) || ( x > 7 && y > 7) 

prodInt :: (Int, Int) -> (Int, Int) -> Int
prodInt v w =   fst v * fst w + snd v * snd w

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor v w = fst v < fst w

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos v w = sqrt (fst w - fst v) + (snd w - snd v)

sumaTerna :: (Num a) => a -> a -> a -> a
sumaTerna x y z = x + y + z

posicPrimerPar :: Int -> Int -> Int -> Int
posicPrimerPar x y z | mod x 2 == 0 = 1
                     | mod y 2 == 0 = 2
                     | mod y 3 == 0 = 3
                     |otherwise = 4

crearPar :: (Num a, Num b) => a -> b -> (a, b)
crearPar x y = (x, y)

invertir :: (Num a, Num b) => (a, b) -> (b, a)
invertir v = (snd v, fst v)