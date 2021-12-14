--  PROGRAMACION FUNCIONAL    BEGO FERRERO   Curso 2021-22    --

{- Enunciado: 
Se trata de encontrar todos los caminos que llevan al saltamontes desde la posici�n donde est� en el jard�n hasta el suelo (altura 0), saltando siempre de flor en flor, desde una flor m�s alta a otra m�s baja. El saltamontes puede cambiar su orientaci�n en cada salto.
(Se muestra una ejecuci�n).
-}

-- Imports 
import System.IO.Unsafe 
import Data.Char ( isAlpha )
-- Tipos descritos en el enunciado --------------------

data Orientacion = N|E|S|W deriving (Eq,Enum,Show,Read)
data Saltamonte = Sal Orientacion Posicion deriving Show
type Posicion = (Fila,Columna)
type Flor = (Altura, Posicion)
type Fila = Int
type Columna = Int
type Altura = Int
type Jardin = [Flor]

type Camino = [Saltamonte]
 




mijardin :: Jardin
mijardin =  matrizPosicionesRec(0,0)

dibujoMijardin :: String
dibujoMijardin= leerArchivo "entrada.txt"

leerArchivo:: String -> String
leerArchivo a= do  
                unsafePerformIO . readFile $ a

countFilas :: Int
countFilas= length(agruparDeNenN countColl arrayValores)-1

countFilas1 :: Int
countFilas1= (head a)+1
            where
              a= stringToArray s 
              s= takeWhile(/='\n') (tail(dropWhile (/='\n') removeBarra))

countColl :: Int
countColl= length(tail a)
            where
              a= stringToArray s 
              s= takeWhile(/='\n') (tail(dropWhile (/='\n') removeBarra))

removeGuion:: String
removeGuion= [ x | x <- dibujoMijardin, not (x `elem` "-") ]

removeBarra:: String
removeBarra= replaceChar '|' ' ' removeGuion

replaceChar :: Char -> Char -> String -> String
replaceChar a b = map $ \c -> if c == a then b else c

stringToArray:: String -> [Int]
stringToArray a = map read $ words a:: [Int]

arrayValores :: [Int]
arrayValores =range a 0 ((length a)-countColl-1)
                where a= stringToArray removeBarra 


range :: [a] -> Int -> Int -> [a]
range l i j = take (j-i+1) (drop i l)

maxFila, maxCol :: Int
maxFila = 4
maxCol = 6


alturas :: [[Altura]]
alturas= agruparDeNenN (countColl+1) (arrayValores)

agruparDeNenN :: Eq a => Int -> [a] -> [[a]]
agruparDeNenN n d
  | d == []           = []
  | length d == n     = [d]
  | otherwise         = (take n d):agruparDeNenN n (drop n d)

getAltura:: Posicion -> Altura
getAltura (f,c)= (alturas!!f)!!(c+1)

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = lft ++ rgt
  where (lft, (_:rgt)) = splitAt idx xs 

matrizPosiciones::[(Altura, (Fila,Columna))]
matrizPosiciones= matrizPosicionesRec(0,0)

matrizPosicionesRec::(Int, Int)-> [(Altura, (Fila,Columna))]
matrizPosicionesRec (f,c)
  | f==countFilas-1 && c==countColl-1     =[( (getAltura(f, c)) , (f, c))]
  | c==countColl-1                        =[( (getAltura(f, c)) , (f, c))] ++ matrizPosicionesRec((f+1),(0))
  | otherwise                             =[( (getAltura(f, c)) , (f, c))] ++ matrizPosicionesRec(f,(c+1))

matrizAlturas::[Altura]
matrizAlturas= matrizAlturasRec(0,0)


matrizAlturasRec::(Int, Int)-> [Altura]
matrizAlturasRec (f,c)
  | f==countFilas-1 && c==countColl-1     =[getAltura(f, c)]
  | c==countColl-1                        =[getAltura(f, c)] ++ matrizAlturasRec((f+1),(0))
  | otherwise                             =[getAltura(f, c)] ++ matrizAlturasRec(f,(c+1))

-- Programa principal (interactivo) ----------------------------
main :: IO()
main= do 
        putStrLn "\nAqui se muestra el jardin con las alturas de las flores (el 0 es el suelo)"
        putStrLn dibujoMijardin
        putStrLn "Dame la orientacion del saltamontes (N, E, S o W):"
        orientacion<-getChar
        if (orientacion/='N' &&  orientacion/='E' && orientacion/='S' && orientacion/='W') then do 
          error(show orientacion ++" no es una orientacion correcta.")
        else do 
          putStrLn "\nEs una orientacion"


        putStr "Dame la posicion del saltamontes, par (fila,columna): "
        t<- getLine
        if (t=="") then
            error("Campo vacio")
        else  do
            let pos= leerParInt t
            putStrLn "Los caminos para llegar al suelo son:"
            imprimirFinal (orientacion, pos)

imprimirFinal :: (Char, Posicion) -> IO ()
imprimirFinal ('N', pos)= imprimir (caminos (Sal N pos))
imprimirFinal ('E', pos)= imprimir (caminos (Sal E pos))
imprimirFinal ('S', pos)= imprimir (caminos (Sal S pos))
imprimirFinal ('W', pos)= imprimir (caminos (Sal W pos))
imprimirFinal (_, _)  = error("Orientacion incorrecta")

leerParInt2:: String -> (Int, Int)
leerParInt2 s= ( ((countFilas-1)- fst (leerParInt2 s)-1) , snd (leerParInt2 s))

leerParInt:: String -> (Int, Int)
leerParInt s
    | (s!!3)==' '                                   =error("Recuerda que es sin espacio")
    | (length s)/=5   || (s!!0)/='(' || (s!!4)/=')' =error("El formato no es correcto")
    | otherwise                                     =((stringToInt [(s!!1)]), (stringToInt [(s!!3)])) 

stringToInt :: [Char] -> Int
stringToInt s= read s :: Int

charToString :: Char -> String
charToString c = [c]
{-
 

leerOrientacion :: IO Orientacion
leerOrientacion = do

-- TO DO: funci�n que recoge la orientaci�n del saltamontes tecleada por el usuario
-}

-- Funciones -----------------------------------------------------

mide ::  Posicion -> Altura
mide(f,c)= getAltura(f,c)

puedeSaltar :: Posicion -> Posicion -> Bool
puedeSaltar (f1, c1) (f2, c2)= (mide (f1,c1)) > (mide(f2,c2))


en_suelo :: Posicion -> Bool
en_suelo (f,c)= (mide (f,c)) == minimo

minimo:: Int
minimo= foldr min 0 (matrizAlturas)

-- caminos j salt = lista de caminos al suelo desde salt en el jard�n j
caminos :: Saltamonte -> [Camino]
caminos (Sal ori pos) 
  | en_suelo pos  = [[Sal ori pos]]
  | null lista      = []
  | otherwise       = map ((Sal ori pos) :) lista
      where  
      lista = concat [(caminos salN) | 
                      oriN <- giros ori,
                      salN <- avanzar (Sal oriN pos)]

-- avanzar j salt = lista de saltamontes en 1 paso desde salt en j
avanzar :: Saltamonte -> [Saltamonte]
avanzar (Sal o (f,c)) 

  | o==N && f>0 && c>=0 && f<countFilas && c<countColl && puedeSaltar (f,c)(f-1,c)          =[Sal o (f-1,c)]
  | o==S && f>=0 && c>=0 && f<(countFilas-1) && c<countColl && puedeSaltar (f,c)(f+1,c)     =[Sal o (f+1,c)]
  | o==E && f>=0 && c>=0 && f<countFilas && c<(countColl-1) && puedeSaltar(f,c) (f,c+1)     =[Sal o (f,c+1)]
  | o==W && f>=0 && c>0 && f<countFilas && c<countColl && puedeSaltar (f,c) (f,c-1)         =[Sal o (f,c-1)]
  | otherwise                                                                               =[]

-- giros ori = lista con las 4 orientaciones desde ori
giros :: Orientacion -> [Orientacion]
giros o = [o,vuelta o, vuelta(vuelta o), vuelta(vuelta(vuelta o))]

vuelta :: Orientacion -> Orientacion  
vuelta W = N
vuelta ori = succ ori

---- impresion ----------------------------------------------------

imprimir:: [Camino] -> IO()
imprimir lisCaminos 
    = if null lisCaminos then putStrLn "No hay solucion"
      else putStrLn (concat (map dibujar lisCaminos))

dibujar :: [Saltamonte] -> String
dibujar [] = []
dibujar [sal] = show sal ++ "\n"
dibujar (sal:resto) = show sal ++ " --> " ++ dibujar resto 

---------------------------------------------------------------------