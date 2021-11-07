data Arbus a = Vac | Nod (Arbus a) a (Arbus a)

{- 1. Dado el siguiente tipo de árboles binarios: data Arbus  = Vac | Nod (Arbus )  (Arbus )
y la siguiente función foldArbus (para aplanar árboles de este tipo) definida: -}

{- (a) Da el tipo más general para foldArbus. -}
foldArbus :: (a1 -> a2 -> a1 -> a1) -> a1 -> Arbus a2 -> a1
foldArbus a e Vac = e
foldArbus a e (Nod ai r ad) = a (foldArbus a e ai) r (foldArbus a e ad) 

{- (b) Define la función numVerif que, dados un predicado (sobre los elementos del árbol) y un árbol, devuelve
el número de nodos del árbol que verifican el predicado. Esta función debe definirse en términos de foldArbus. 
-}
numVerif :: Arbus a -> Int
numVerif arbus= length(aplanar arbus) 

aplanar :: Arbus a -> [a]
aplanar Vac = []
aplanar (Nod ai r ad) = aplanar ai ++ [r] ++ aplanar ad

{- (c) Dada la declaración de tipo: -} 
type ArPares a= Arbus (a, a) {-
define una función numParesCorr que, dado un árbol del tipo ArPares determine, empleando numVerif, el
número de nodos correctos del árbol. Un par (x,y) es correcto si x es menor que y.
-}
{- TODO: Esto esta mal
numParesCorr :: ArPares a-> Bool
numParesCorr :: Arbus a1 -> Arbus a2 -> Bool
numParesCorr a1 a2= (numVerif a1) > (numVerif a2)
-}

-- 2. Dada la siguiente definición para árboles generales (con un número arbitrario de subárboles): 
data ArGen alpha = N alpha [ArGen alpha]
-- (a) Escribe los términos de este tipo correspondientes a los árboles ar1 y ar2 de abajo.

--(b) Define funciones de recorrido del árbol en preorden y en postorden.
--(c) Define una función esta que decida si un elemento está en un árbol o no.