<<<<<<< HEAD
-- 7 --------------------------------------------------------------
type Direccion = (Persona, Dir, Ciudad)
type Nombre = String
type Apellido = String
type Ciudad = String
data Persona = Per Nombre Apellido
data Dir = Calle String Int | Casa String 

dirJon:: Direccion
dirJon= ((Per "Jon" "Prieto"), (Casa "Enea"), "Orio")

dirMiren:: Direccion
dirMiren= ((Per "Miren" "Artola"), (Calle "Aldamar" 15), "Donostia")

escribir:: [Direccion] -> IO()
escribir ad= putStr (escribirArrayDirecciones ad)

escribirArrayDirecciones :: [Direccion] -> String
escribirArrayDirecciones []= ""
escribirArrayDirecciones (ad:ads)= (escribirDireccion ad) ++ "\n" ++escribirArrayDirecciones(ads)

escribirPer :: Persona -> String
escribirPer (Per n a)= n ++" "++a

escribirDir :: Dir -> String
escribirDir (Calle s i)= "c/ "++s++" "++(show i) 
escribirDir (Casa s)= "casa "++s 

escribirDireccion :: Direccion -> String
escribirDireccion (p, d, c) =  escribirPer(p) ++" "++ escribirDir(d) ++", "++ c

-- 8 --------------------------------------------------------------
data Elemento = E String Int deriving Show

instance Eq Elemento where
    (E s1 _) == (E s2 _) = s1==s2

instance Ord Elemento where
    (E _ c1) <= (E _ c2) = c1 <= c2

-- Suponiendo que no hay negativos
dosMayores :: [Elemento] -> (Elemento, Elemento)
dosMayores []= error ("Lista vacia")
dosMayores (x:xs:xss)
    | x <= xs                 = dosMayoresRec xss x xs
    | otherwise               = dosMayoresRec xss xs x
dosMayores [_]= error("Lista Incorrecta")

dosMayoresRec :: [Elemento] -> Elemento -> Elemento -> (Elemento, Elemento)
dosMayoresRec[] max1 max2= (max1, max2)
dosMayoresRec(x:xs) max1 max2
    | x >= max1     = dosMayoresRec xs x max1
    | x >= max2     = dosMayoresRec xs max1 x
=======
-- 7 --------------------------------------------------------------
type Direccion = (Persona, Dir, Ciudad)
type Nombre = String
type Apellido = String
type Ciudad = String
data Persona = Per Nombre Apellido
data Dir = Calle String Int | Casa String 

dirJon:: Direccion
dirJon= ((Per "Jon" "Prieto"), (Casa "Enea"), "Orio")

dirMiren:: Direccion
dirMiren= ((Per "Miren" "Artola"), (Calle "Aldamar" 15), "Donostia")

escribir:: [Direccion] -> IO()
escribir ad= putStr (escribirArrayDirecciones ad)

escribirArrayDirecciones :: [Direccion] -> String
escribirArrayDirecciones []= ""
escribirArrayDirecciones (ad:ads)= (escribirDireccion ad) ++ "\n" ++escribirArrayDirecciones(ads)

escribirPer :: Persona -> String
escribirPer (Per n a)= n ++" "++a

escribirDir :: Dir -> String
escribirDir (Calle s i)= "c/ "++s++" "++(show i) 
escribirDir (Casa s)= "casa "++s 

escribirDireccion :: Direccion -> String
escribirDireccion (p, d, c) =  escribirPer(p) ++" "++ escribirDir(d) ++", "++ c

-- 8 --------------------------------------------------------------
data Elemento = E String Int deriving Show

instance Eq Elemento where
    (E s1 _) == (E s2 _) = s1==s2

instance Ord Elemento where
    (E _ c1) <= (E _ c2) = c1 <= c2

-- Suponiendo que no hay negativos
dosMayores :: [Elemento] -> (Elemento, Elemento)
dosMayores []= error ("Lista vacia")
dosMayores (x:xs:xss)
    | x <= xs                 = dosMayoresRec xss x xs
    | otherwise               = dosMayoresRec xss xs x
dosMayores [_]= error("Lista Incorrecta")

dosMayoresRec :: [Elemento] -> Elemento -> Elemento -> (Elemento, Elemento)
dosMayoresRec[] max1 max2= (max1, max2)
dosMayoresRec(x:xs) max1 max2
    | x >= max1     = dosMayoresRec xs x max1
    | x >= max2     = dosMayoresRec xs max1 x
>>>>>>> 30c390c93e1d5bbcd962a727c11a45e4049c8081
    | otherwise     = dosMayoresRec xs max1 max2