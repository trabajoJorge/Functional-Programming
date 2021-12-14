-- 2 --------------------------------------------------------------
data ArGen a = N a [ArGen a] 
-- a)
ar1::ArGen Integer
ar1= N 25 [(N 35 []), (N 45 []), (N 55 [])] 
ar2::ArGen Integer
ar2= N 25 [(N 12 []), (ar1), (N 36 [N 52 []])] 

-- b)
recorridoPreorden:: ArGen a-> [a]
recorridoPreorden (N i h)
    | null h       = [i]
    | otherwise     = foldl (++) [i] (map recorridoPreorden h)

recorridoPostorden:: ArGen a-> [a]
recorridoPostorden (N i h)
    | null h       = [i]
    | otherwise     = foldr (++) [i] (map recorridoPostorden h)

-- c)
esta:: Eq a => ArGen a-> a -> Bool
esta a= estaReal(recorridoPostorden a)

estaReal:: Eq a => [a]-> a -> Bool
estaReal [] _ = False
estaReal (x:xs) n
    | x==n      =True
    | otherwise =estaReal xs n


-- 3 --------------------------------------------------------------
data Arbol a b = Hoja a | Nodo (Arbol a b) b (Arbol a b)
-- a)
type expArit= Arbol Int String
-- b)
aExp= Nodo (Nodo (Hoja 9) "-" (Nodo (Hoja 10) "+" (Hoja 6)))"+"(Nodo (Hoja 3) "*" (Hoja 5))
-- c)
-- TODO: pidelo

