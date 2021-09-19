import Data.Set ( fromList, toList )
import Data.List 

-- Ejercicio 1
quitaUno :: Eq a => a -> [a] -> [a] -- Eq para que la a sea de un tipo generico que se pueda igualar.
quitaUno _ [] = []
quitaUno x (y:ys)   | x == y    = quitaUno x ys
                    | otherwise = y : quitaUno x ys

-- Ejercicio 2
quitaRep ::  Ord a =>  [a] -> [a]
quitaRep = toList . fromList

-- Ejercicio 3
eliminarDuplicados :: (Eq a) => [a] -> [a]
eliminarDuplicados [] = [] 
eliminarDuplicados [x] = [x]
eliminarDuplicados (x:xs) = x : [ k | k <- eliminarDuplicados xs, k /=x ]

-- Ejercicio 4
dif::Eq a=>[a]->[a]->[a]
dif x [] = x
dif x (y:ys) = dif (quitaUno y x) ys

--Ejercicio 5
perm::Eq a=>[a]->[a]->Bool
perm x y = (dif x y == []) && (dif y x == [])

--Ejercicio 6
aDecimal [Int] -> Int
a