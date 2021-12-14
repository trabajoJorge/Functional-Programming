


{- 1. Dado el siguiente tipo de árboles binarios: -}
data Arbus a = Vac | Nod (Arbus a) a (Arbus a)
{- y la siguiente función foldArbus (para aplanar árboles de este tipo) definida: -}

{- (a) Da el tipo más general para foldArbus. -}
foldArbus :: (a1 -> a2 -> a1 -> a1) -> a1 -> Arbus a2 -> a1
foldArbus a e Vac = e
foldArbus a e (Nod ai r ad) = a (foldArbus a e ai) r (foldArbus a e ad) 

{- (b) Define la función numVerif que, dados un predicado (sobre los elementos del árbol) y un árbol, devuelve
el número de nodos del árbol que verifican el predicado. Esta función debe definirse en términos de foldArbus. 
-}

numVerif::(a->Bool)->Arbus a->Int
numVerif _ Vac = 0
numVerif f (Nod left node right)
    | f node = value + 1
    | otherwise = value 
        where value = (numVerif f left) + (numVerif f right)

numVerif1 :: Arbus a -> Int
numVerif1 arbus= length(aplanar arbus) 

aplanar :: Arbus a -> [a]
aplanar Vac = []
aplanar (Nod ai r ad) = aplanar ai ++ [r] ++ aplanar ad

{- (c) Dada la declaración de tipo: -} 
type ArPares a= Arbus (a, a) {-
define una función numParesCorr que, dado un árbol del tipo ArPares determine, empleando numVerif, el
número de nodos correctos del árbol. Un par (x,y) es correcto si x es menor que y.
-}
comparar::Ord a=>(a, a)->Bool 
comparar (a,b) = a < b

numParesCorr::Ord a=>ArPares a->Int
numParesCorr arbol = numVerif comparar arbol

{- 2. Dada la siguiente definición para árboles generales (con un número arbitrario de subárboles): -}
data ArGen a = N a [ArGen a]

-- (a) Escribe los términos de este tipo correspondientes a los árboles ar1 y ar2 de abajo.
ar1::ArGen Integer
ar1 = N 25 [N 35 [], N 45 [], N 55 []]
ar2 :: ArGen Integer
ar2 = N 20 [N 12 [], ar1, N 36 [N 52 []]]

-- (b) Define funciones de recorrido del árbol en preorden y en postorden.
preorden::ArGen a->[a]
preorden (N x xs)
    | null xs = [x]
    | otherwise = foldl (++) [x] (map preorden xs)

postorden::ArGen a->[a]
postorden (N x xs)
    | null xs = [x]
    | otherwise = foldr (++) [x] (map postorden xs)

-- (c) Define una función esta que decida si un elemento está en un árbol o no.
isInTree::Eq a =>a->ArGen a->Bool
isInTree e arb = 1 <= length (filter (==e) (postorden arb))

-- 3. Supongamos el siguiente tipo de árboles binarios con información de dos tipos en nodos internos y hojas:
data Arbol a1 a2 = Hoja a1 | Nodo (Arbol a1 a2) a2 (Arbol a1 a2)
--(a) Define el tipo de las expresiones aritméticas en términos de este tipo de árboles.
type ExpArit= Arbol Int String

--(b) Escribe el término correspondiente a la expresión exp1 = (9 - (10+6)) + (3*5)
arbol:: ExpArit
arbol= Nodo (Nodo (Hoja 9) "+" (Nodo (Hoja 10) "+" (Hoja 6))) "+" (Nodo (Hoja 3) "*" (Hoja 5))


--(c) Declara Arbol como instancia de la clase Show, de forma que exp1 se vea en pantalla así: 
-- NO ME HA SALIDO



