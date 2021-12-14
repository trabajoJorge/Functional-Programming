-- 1 --------------------------------------------------------------
data Circulo = Cir Radio
data TrianguloRect = Tri Base Altura
data Cuadrilatero = Cuad Lado | Rect Base Altura

type Radio = Float 
type Lado = Float
type Altura = Float 
type Base = Float 

-- a)
class FigurasPlanas a where
    perimetro, area:: a -> Float

-- b)
instance FigurasPlanas Circulo where
    perimetro(Cir r)= 2*pi*r
    area (Cir r) = pi * (r ** 2)

instance FigurasPlanas TrianguloRect where
    perimetro (Tri b h) = b + sqrt ((b**2) + 4 * (h**2))
    area (Tri b h) = (b * h) / 2

instance FigurasPlanas Cuadrilatero where
    perimetro (Cuad l) = 4 * l
    perimetro (Rect b h) = (2 * b) + (2 * h)
    area (Cuad l) = l ** 2
    area (Rect b h) = b * h

-- 2 --------------------------------------------------------------
data CatLista a = Nil | Unit a | Conc (CatLista a) (CatLista a)

-- a)
instance Eq a => Eq (CatLista a) where 
    (==) a b = (CatListatoList a) == (CatListatoList b)

catListaCatListatoList :: CatLista a -> [a]
catListaCatListatoList (Nil) = []
catListaCatListatoList (Unit a) = [a]
catListaCatListatoList (Conc a b) = (catListaCatListatoList a) ++ (catListaCatListatoList b)
catListaCatListatoList _ = error("La lista es incorrecta")

-- b)
primero :: CatLista a -> a
primero (Unit a)    = a
primero (Conc a b)  = primero a
primero _ = error("La lista es incorrecta")
{-
-- 3 --------------------------------------------------------------
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

    inter :: Conj a -> Conj a -> Conj a
    inter (Co c1) (Co c2) = Co (intersect c1 c2)

    intersect :: Ord a => [a] -> [a] -> [a]
    intersect xs ys = intersectSorted (sort xs) (sort ys)

    dif :: Conj a -> Conj a -> Conj a
    dif (Co c1) (Co c2) = c1 // c2

    card :: Conj a -> Int 
    card (Co c) = lenght c 

    subConj :: Conj a -> Conj a -> Bool
    subConj (Co c1) (Co c2) = isInfixOf c1 c2

    hacerConj :: [a] -> Int
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

    inter :: Conj a -> Conj a -> Conj a
    inter (Co c1) (Co c2) = Co (intersect c1 c2)

    intersect :: Ord a => [a] -> [a] -> [a]
    intersect xs ys = intersectSorted (sort xs) (sort ys)

    dif :: Conj a -> Conj a -> Conj a
    dif (Co c1) (Co c2) = c1 // c2

    subConj :: Conj a -> Conj a -> Bool
    subConj (Co c1) (Co c2) = isInfixOf c1 c2

    hacerConj :: [a] -> Conj a
    hacerConj x = Co (map head . group . sort x)

    mapConj :: (a -> b) -> Conj a -> Conj b 
    mapConj f (Co c) = map f c

    filterConj :: (a->Bool) -> Conj a -> Conj a 
    filterConj f (Co c) = filter f c

    foldConj :: (a -> b -> b) -> b -> Conj a -> b
    foldConj f x (Co c) = fold f x c-}