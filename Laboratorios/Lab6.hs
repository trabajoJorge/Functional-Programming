{-
Sexto bloque de ejercicios de la asignatura Programación Funcional
Autor: Aitor Iglesias
-}

---------------------- EJERCICIO 1 ----------------------

data Circulo = Cir Radio 
data TrianguloRect = Tri Base Altura 
data Cuadrilatero = Cuad Lado | Rect Base Altura 
 
type Radio = Float   
type Lado = Float 
type Altura = Float   
type Base = Float 

class FigurasPlanas a where
    perimetro, area :: a -> Float

instance FigurasPlanas Circulo where
    perimetro (Cir rad) = 2 * pi * rad
    area (Cir rad) = pi * (rad ** 2)

instance FigurasPlanas TrianguloRect where
    perimetro (Tri bas alt) = bas + sqrt ((bas**2) + 4 * (alt**2))
    area (Tri bas alt) = (bas * alt) / 2

instance FigurasPlanas Cuadrilatero where
    perimetro (Cuad lad) = 4 * lad
    perimetro (Rect bas alt) = (2 * bas) + (2 * alt)
    area (Cuad lad) = lad ** 2
    area (Rect bas alt) = bas * alt

---------------------- EJERCICIO 2 ----------------------


data CatLista a = Nil | Unit a | Conc (CatLista a) (CatLista a)

toList :: CatLista a -> [a]
toList (Nil) = []
toList (Unit a) = [a]
toList (Conc a b) = (toList a) ++ (toList b)

instance Eq a => Eq (CatLista a) where 
    (==) a b = (toList a) == (toList b)


primero :: CatLista a -> a
primero (Nil)       = error"Esa lista es vacia"
primero (Unit a)    = a
primero (Conc a b)  = primero a

---------------------- EJERCICIO 3 ----------------------

module Conjunto1
where
    import Data.List

    data Conj a = Co [a]
	
    instance (Eq a) => Eq (Conj a) where
    (Co c1) == (Co c2) = (c1 == c2)

    instance (Show a) => Show (Conj a) where
    show (Co c) = "{" ++ (tail s) ++ "}" 
        where s = init (show c)

    vacio :: Conj a
    vacio = Co []

    simple :: a -> Conj a
    simple x = Conj [x]

    miembro :: a -> Conj a -> Bool
    miembro x (Co c) = member x c

    union :: Conj a -> Conj a -> Conj a
    union (Co c1) (Co c2) = Co (c1 ++ c2)

    --inter :: Conj a -> Conj a -> Conj a
    --inter (Co c1) (Co c2) =
    
    dif :: Conj a -> Conj a -> Conj a
    dif (Co c1) (Co c2) = c1 // c2
    
    --card :: Conj a -> Int 
    --card (Co c) = 
    
    subConj :: Conj a -> Conj a -> Bool
    subConj (Co c1) (Co c2) = isInfixOf c1 c2

    hacerConj :: [a] -> Conj a
    hacerConj x = Co x
    
    mapConj :: (a -> b) -> Conj a -> Conj b 
    mapConj f (Co c1) (Co c2) = map f c1 c2
    
    filterConj :: (a->Bool) -> Conj a -> Conj a 
    filterConj f (Co c1) (Co c2) = filter f c1 c2
    
    foldConj :: (a -> b -> b) -> b -> Conj a -> b
    foldConj f x (Co c) = fold f x c
	


module Conjunto2
where
    import Data.List

    data Conj a = Co [a]
	
    instance (Eq a) => Eq (Conj a) where
    (Co c1) == (Co c2) = (c1 == c2)

    instance (Show a) => Show (Conj a) where
    show (Co c) = "{" ++ (tail s) ++ "}" 
        where s = init (show c)

    vacio :: Conj a
    vacio = Co []

    simple :: a -> Conj a
    simple x = Conj [x]

    miembro :: a -> Conj a -> Bool
    miembro x (Co c) = member x c

    union :: Conj a -> Conj a -> Conj a
    union (Co c1) (Co c2) = Co (map head . group . sort (c1 ++ c2))

    --inter :: Conj a -> Conj a -> Conj a
    --inter (Co c1) (Co c2) =
    
    dif :: Conj a -> Conj a -> Conj a
    dif (Co c1) (Co c2) = c1 // c2
    
    --card :: Conj a -> Int 
    --card (Co c) = 
    
    subConj :: Conj a -> Conj a -> Bool
    subConj (Co c1) (Co c2) = isInfixOf c1 c2

    hacerConj :: [a] -> Conj a
    hacerConj x = Co (map head . group . sort x)
    
    mapConj :: (a -> b) -> Conj a -> Conj b 
    mapConj f (Co c) = map f c
    
    filterConj :: (a->Bool) -> Conj a -> Conj a 
    filterConj f (Co c) = filter f c
    
    foldConj :: (a -> b -> b) -> b -> Conj a -> b
    foldConj f x (Co c) = fold f x c