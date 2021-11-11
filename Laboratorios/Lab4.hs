
{- 1. Da el tipo más general posible de las siguientes funciones. Comprueba después qué responde el sistema como
    tipo inferido. 
    ghci> constante x y = x
    ghci> :t constante        
    constante :: p1 -> p2 -> p1
    ghci> subst f g x = f x (g x) 
    ghci> :t subst
    subst :: (t1 -> t2 -> t3) -> (t1 -> t2) -> t1 -> t3       
    ghci> aplicar f x = f x 
    ghci> :t aplicar
    aplicar :: (t1 -> t2) -> t1 -> t2
    ghci> fliparg f x y = f y x
    ghci> :t fliparg
    fliparg :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
    ghci> cuadrado x = x * x
    ghci> :t cuadrado  
    cuadrado :: Num a => a -> a
    ghci> rara f g = g ( f g)
    ghci> :t rara
    rara :: ((t1 -> t2) -> t1) -> (t1 -> t2) -> t2
    ghci>
    ghci> masRara f = f f
    
    !!SALTA ERRORR!!
    ghci> masRara f = f f 
    
    <interactive>:17:15: error:
        * Couldn't match expected type `t' with actual type `t -> t1'
        * In the first argument of `f', namely `f'
          In the expression: f f
          In an equation for `masRara': masRara f = f f
        * Relevant bindings include
            f :: t -> t1 (bound at <interactive>:17:9)
            masRara :: (t -> t1) -> t1 (bound at <interactive>:17:
-}

{- 2. Dada la siguiente definición para la función casos, pregunta :t casos y compáralo con el tipo de la función
    predefinida either.
        ghci> casos (f, g) (Left x) = f x  
        ghci> :t casos                       
        casos :: (t1 -> t2, b1) -> Either t1 b2 -> t2
        ghci> casos (f, g) (Right y) = g y 
        ghci> :t casos
        casos :: (a1, t1 -> t2) -> Either a2 t1 -> t2
-}

{- 3. Dada la función (y las del anterior ejercicio):
        ghci> casos (f, g) (Left x) = f x
        ghci> casos (f, g) (Right y) = g y 
        ghci> h = casos ((>0), (==) 'a')
    evalúa las expresiones:   
        ghci> h (Left 7)
        *** Exception: <interactive>:5:1-28: Non-exhaustive patterns in function casos

        ghci> h (Left (-2)) 
        *** Exception: <interactive>:5:1-28: Non-exhaustive patterns in function casos
        
        ghci> h (Right 'b')
        False
        ghci> h (Right 'a')
        True
-}

{-  4. Con el nuevo tipo:
        ghci> data TipoBobo = Uno | Dos | Tres deriving (Show, Eq)
    evalúa las expresiones:
        ghci> Tres
        Tres
        ghci> Uno == Uno
        True
        ghci> Uno == Dos
        False
        ghci> Uno < Dos
        <interactive>:5:5: error:
            * No instance for (Ord TipoBobo) arising from a use of `<'
            * In the expression: Uno < Dos
            In an equation for `it': it = Uno < Dos
-}

-- 5. Con el tipo siguiente: data MisEnteros = Mi Int y la concreción (o instancia) siguiente de clase:
data MisEnteros = Mi Int
instance Show MisEnteros where
    show (Mi n)
        | n>0 = "+" ++ show n
        | n<0 = "%" ++ show (abs n)
        | otherwise = "<0>" 
{-  
    ghci> Mi (-23)
    %23
    ghci> Mi 756
    +756
    ghci>
-}

-- 6. Con el tipo siguiente (con show derivado): 
data TusEnteros = Tu Int deriving Show
-- evalúa las siguientes expresiones:
{-  ghci> Tu 756
    Tu 756
    ghci> Tu (-23)
    Tu (-23)
    ghci> Tu 0
    Tu 0
-}

-- 7. Dados los siguientes tipos de datos:
type Direccion = (Persona, Dir, Ciudad)
type Nombre = String
type Apellido = String
type Ciudad = String
data Persona = Per Nombre Apellido deriving Show
data Dir = Calle String Int | Casa String deriving Show

dirJon:: Direccion
dirJon= (Per "Jon" "Prieto ", Casa "Enea ", "Orio")

dirMiren::Direccion
dirMiren = (Per "Miren" "Artola ", Calle "Aldamar" 15, "Donostia")

dirToString :: Dir -> [Char]
dirToString (Calle s value) = "c/ " ++ s ++", "++ show value
dirToString (Casa s) =  "casa " ++ s

printDir :: Dir -> [Char]
printDir (Calle nombreCalle numero)="Calle " ++ nombreCalle ++" "++ show numero ++ " "
printDir (Casa nombreCasa) =  "Casa " ++ nombreCasa

printPer :: [Char] -> [Char] -> [Char]
printPer name subName= name ++ ", " ++ subName

printDireccion::Direccion->String
printDireccion (Per name subName, dir, city) =  printPer name subName ++ printDir dir ++ city

printDirecciones:: [Direccion]->IO() 
printDirecciones []= putStr ""
printDirecciones (x:xs)=  putStrLn(printDireccion x) >> printDirecciones(xs)

-- TODO: Falta este
-- 8. Dado el siguiente tipo algebraico data Elemento = E String Int deriving Show que permite representar
-- palabras con un contador asociado a cada una. 
data Elemento = E String Int deriving Show

instance Eq Elemento where 
    (E s1 _) == (E s2 _) = s1 == s2

instance Ord Elemento where 
    (E s1 _) <= (E s2 _) = s1 <= s2

dosMayores :: [Elemento] -> (Elemento, Elemento)
dosMayores (x:xs:xss)
    | x > xs=       dosMayoresReal xss (x, xs)
    | otherwise=    dosMayoresReal xss (xs, x)

dosMayoresReal:: [Elemento] -> (Elemento, Elemento) -> (Elemento, Elemento)
dosMayoresReal [] (e1, e2)= (e1, e2)
dosMayoresReal (x:xs)(e1, e2)
    | x > e1                = dosMayoresReal xs(x, e1)
    | x < e1 && x < e2      = dosMayoresReal xs (e1, x)
    | otherwise             = dosMayoresReal xs (e1, e2)


-- 9. Define el tipo algebraico data Racional = Int :/ Int como:
data Racional = Int :/ Int

instance Eq Racional where
    (n1:/d1) == (n2:/d2) = n1*d2 == n2*d1

instance Ord Racional where
    (n1:/d1) <= (n2:/d2) = n1*d2 <= n2*d1

calculateMCD :: Int -> Int -> Int
calculateMCD a 0 = a
calculateMCD a b = calculateMCD b (rem a b)


instance Show Racional where
    show (num:/denom)
        | denom == 1    = show (div num (calculateMCD num denom))
        | otherwise     = (show (div num (calculateMCD num denom)) ++ "/" ++ show  (div denom (calculateMCD num denom)))

data Operador = Mas | Menos | Prod | Div
data ExprArit = N Int | Apli Operador ExprArit ExprArit 

evaluar:: ExprArit -> Int
evaluar (N n)= n
evaluar (Apli o e1 e2)= operar o (evaluar e1) (evaluar e2)

operar::Operador->Int->Int->Int
operar Mas o1 o2      = o1 + o2
operar Menos o1 o2    = o1 - o2
operar Prod o1 o2     = o1 * o2
operar Div o1 o2      = div o1 o2

instance Eq ExprArit where
    e1 == e2 = evaluar e1 == evaluar e2


showOperador :: Operador -> [Char]
showOperador Mas    = " + "
showOperador Menos  = " - "
showOperador Prod   = " * "
showOperador Div    = " div "

showAux :: ExprArit -> [Char]
showAux (N n)= show n
showAux (Apli o e1 e2)= "(" ++ showAux e1 ++ showOperador o ++ showAux e2 ++ ")"

showLast :: ExprArit -> [Char]
showLast (Apli o e1 e2)= tail (init(showAux (Apli o e1 e2)))

instance Show ExprArit where
    show= showLast