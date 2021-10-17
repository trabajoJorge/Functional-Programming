------------------------------------------------------------------
--  PRACTICA: CALENDARIO           --               PF  2020-2021

--  Nombres:  Jorge Iglesias 
--            Asier Cruz 
------------------------------------------------------------------

-- Llamada principal es:  printCalendario c n
-- donde c = columnas (3 o 4) y
--       n = año cuyo calendario deseamos imprimir
------------------------------------------------------------------

module Calendarios where
import Distribution.Simple.Utils (xargs)

type Dibujo = [Linea] -- cada dibujo es una lista de lineas

type Linea = [Char] -- cada linea es una lista de caracteres

type Year = Int

type Columna = Int -- es 3 o 4

-- Para imprimir un dibujo en pantalla:
printDibujo :: Dibujo -> IO ()
printDibujo dib = do
  putStr "\n"
  (putStr . concat . map (++ "\n")) dib

-- Imprime, con un numero de columnas, el calendario de un a�o:
printCalendario :: Columna ->  Year -> IO()
printCalendario c a = printDibujo (calendario c a)

-- Dibujo de un calendario (en c columnas) de un a�o dado:
calendario :: Columna -> Year -> Dibujo
calendario c  =  bloque c . map dibujomes . meses

---------------------------------------------------
--  Define las siguientes funciones sobre dibujos:
---------------------------------------------------

-- comprueba que las lineas de un dibujo tienen igual longitud,
-- debe dar un mensaje de error si el dibujo es vacío ([]).
dibEsCorrecto :: Dibujo -> Bool
dibEsCorrecto (d:ds)
  | null d                          = error("El dibujo no es correcto")
  | null ds                         = True
  | length d == length (head ds)    = dibEsCorrecto ds
  | otherwise                       = False

-- comprueba que los dibujos de la lista dada son correctos y
-- ademas tienen todos las mismas dimensiones.
listaDibCorrectos :: [Dibujo] -> Bool
listaDibCorrectos (m : ms)
  | null ms = dibEsCorrecto(m)
  | not(dibEsCorrecto(m)) = error("La lista de dibujos no es correcta")
  | (ancho m == ancho (head ms)) && (alto m == alto (head ms)) = (listaDibCorrectos ms) && (dibEsCorrecto m)
  | otherwise = False

-- Pre: dib es un dibujo correcto.
-- alto dib da la altura de dib.
alto :: Dibujo -> Int
alto d = length d

-- ancho :: Dibujo -> Int
-- Pre: dib es un dibujo correcto.
-- ancho dib da la anchura de dib.
ancho :: Dibujo -> Int
ancho (d : ds) = length d


-- Precondicion: los dibujos d1 y d2 tienen la misma anchura.
-- sobre d1 d2 pone el dibujo d1 sobre el dibujo d2.
sobre :: Dibujo -> Dibujo -> Dibujo
sobre d1 d2 = d1 ++ d2

-- Precondicion: los dibujos d1 y d2 tienen la misma altura.
-- alLado d1 d2 da un dibujo con d1 a la izquierda de d2.
-- alLado :: Dibujo -> Dibujo -> Dibujo
alLado :: Dibujo -> Dibujo -> Dibujo
alLado d1 d2= zipWith (++) (d1) (d2)

-- apila s da el dibujo obtenido apilando todos los elementos de s
--         (el primero de s queda en la cima de la pila).
-- Si s no es una lista de dibujos correctos debe dar error.
apilar :: [Dibujo] -> Dibujo
apilar ([]:xs)= error("La lista de dibujos es incorrecta")
apilar (x:[])= x
apilar (x:xs)= x++ apilar xs

apilar2 :: [Dibujo] -> Dibujo
apilar2 (x:xs)
  | listaDibCorrectos(x:xs)   = foldr sobre x xs
  | otherwise                 = error("La lista de dibujos es incorrecta")



-- extender :: [Dibujo] -> Dibujo
-- extiende s da el dibujo obtenido al extender todos los elementos --            de s (el primero de s queda el m�s a la izquierda).
-- Si s no es una lista de dibujos correctos debe dar error.
extender :: [Dibujo] -> Dibujo
extender (x:xs)
  | listaDibCorrectos(x:xs)   = foldl alLado x xs
  | otherwise                 = error("La lista de dibujos es incorrecta")


-- dibBlanco :: (Int,Int) -> Dibujo
-- Precondicion: al>0 && an>0.
-- dibBlanco (al,an) devuelve el dibujo de caracteres blancos con
--                   altura al y anchura an
dibBlanco :: (Int,Int) -> Dibujo
dibBlanco (1,y) = [stringBlancos y]
dibBlanco (x,y) = [stringBlancos y] ++ dibBlanco(x-1, y)

stringBlancos :: Int -> [Char]
stringBlancos 1= " "
stringBlancos y= " "++stringBlancos(y-1)

-- bloque n lisDib es el dibujo formado al agrupar de n en n los
--               dibujos de lisDib, extender cada sublista
--               y luego apilar los resultados.
bloque::Int->[Dibujo]->Dibujo
bloque n d= map (++ "    ") b
            where b= bloque2 n d

bloque2::Int->[Dibujo]->Dibujo
bloque2 n d
  | listaDibCorrectos d     = apilar(agruparDeNenN n d)
  | otherwise               = error("Lista incorrecta")

agruparDeNenN :: Int -> [Dibujo] -> [Dibujo]
agruparDeNenN n d
  | length d == n     = [extender(d)]
  | otherwise         = extender(take n d) : agruparDeNenN n nuevaLista
                        where nuevaLista= drop n d


------------------------------------------------------------------
-- Define constantes y funciones para calcular y dibujar los meses
------------------------------------------------------------------

-- meses ::  Year -> [(String, Year, Int, Int)]
-- meses n devuelve una lista de 12 elementos con los datos
--         relevantes de cada uno de los meses del a�o n:
--         (nombre_mes, n, primer_d�a_mes, longitud_mes)
meses ::  Year -> [(String, Year, Int, Int)]
meses a= mesesRecursiva 12 nombresmeses a (pdias a) (diasMeses a) 

mesesRecursiva :: Int -> [String] -> Int -> [Int] -> [Int] -> [(String, Year, Int, Int)]
mesesRecursiva 1 (m:ms) a (d:ds) (n:ns)= [(m, a, d, n)]
mesesRecursiva r (m:ms) a (d:ds) (n:ns)= (m, a, d, n) : mesesRecursiva (r-1) ms a ds ns

diasMeses:: Int -> [Int]
diasMeses a = [31,feb,31, 30, 31, 30, 31,31,30,31,30,31]
                where
                  feb= if esBisiesto a then 29 else 28

-- dibujomes ::(String, Year, Int, Int) -> Dibujo
-- dibujomes (nm,a,pd,lm) devuelve un dibujo de dimensiones 10x25
-- formado por el titulo y la tabla del mes de nombre nm y a�o a.
-- Necesita como par�metros: pd=primer dia y lm=longitud del mes.

dibujomes ::(String, Year, Int, Int) -> Dibujo
dibujomes (m, a, d, n)=  (" " ++ m ++ " " ++ show a ++blancos " " (24 - (length m + 6))) : "                         ": " Lu Ma Mi Ju Vi Sa Do    " : bloque 7(fechas d n)
blancos :: String -> Int -> String
blancos b a
  | a == 1 = " " ++ b
  | otherwise = blancos (" " ++ b) (a-1)

-- ene1 a devuelve el dia de la semana del 1 de enero del a�o a
--        siendo 1=lunes, 2=martes, ..., 6=sabado, 0=domingo
ene1 :: Year -> Int
ene1 a = mod (a + div (a-1) 4 - div (a-1) 100 + div (a-1) 400) 7

-- pdias :: Int -> [Int]
-- pdias a  devuelve una lista con 12 dias que son los dias de la
--          semana en que comienza cada mes del a�o a siendo
--          1=lunes, 2=martes, ..., 6=sabado y 7=domingo
-- Ejemplo: pdias 2019 es [2,5,5,1,3,6,1,4,7,2,5,7]
pdias :: Int -> [Int]
pdias a= [ene, feb, mar, abr, may, jun, jul, ago, sep, oct, nov, dic]
          where 
            ene= ene1 a
            feb= if ene + 3 > 7 then ene + 3 - 7 else ene + 3
            mar=  if esBisiesto a then 
                    if feb + 1 > 7 then 
                      feb+1 -7 
                    else
                      feb+1 
                  else feb
            abr= if mar + 3 > 7 then mar + 3 - 7 else mar + 3
            may= if abr + 2 > 7 then abr + 2 - 7 else abr + 2
            jun= if may + 3 > 7 then may + 3 - 7 else may + 3
            jul= if jun + 2 > 7 then jun + 2 - 7 else jun + 2
            ago= if jul + 3 > 7 then jul + 3 - 7 else jul + 3
            sep= if ago + 3 > 7 then ago + 3 - 7 else ago + 3
            oct= if sep + 2 > 7 then sep + 2 - 7 else sep + 2
            nov= if oct + 3 > 7 then oct + 3 - 7 else oct + 3
            dic= if nov + 2 > 7 then nov + 2 - 7 else nov + 2
            

nombresmeses :: [String]
nombresmeses = ["Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio", "Agosto","Septiembre","Octubre","Noviembre","Diciembre"]

-- fechas :: Int -> Int -> [Dibujo]
-- fechas pd lm da una lista de 42 dibujos de 1*3 (alguno blanco)
--              con los dias de un mes cuyo primer dia de semana
--              es pd y cuya longitud de mes es lm

{- Ejemplo:
fechas 3 30
[["   "],["   "],["  1"],["  2"],["  3"],["  4"],["  5"],["  6"],["  7"],["  8"],["  9"],[" 10"],[" 11"],[" 12"],[" 13"],[" 14"],[" 15"],[" 16"],[" 17"],[" 18"],[" 19"],[" 20"],[" 21"],[" 22"],[" 23"],[" 24"],[" 25"],[" 26"],[" 27"],[" 28"],[" 29"],[" 30"],["   "],["   "],["   "],["   "],["   "],["   "],["   "],["   "],["   "],["   "]]
-}
fechas :: Int -> Int -> [Dibujo]
fechas x y= fechasRecursiva  x y 1 42

fechasRecursiva:: Int->Int->Int->Int -> [Dibujo]
fechasRecursiva pd lm diaActual r 
  | pd > 1                                      = ["   "]:fechasRecursiva (pd-1) lm diaActual (r-1)  
  | pd==1 && diaActual<10                       = ["  "++show diaActual]:fechasRecursiva pd lm (diaActual+1) (r-1) 
  | pd==1 && diaActual>=10 && diaActual<=lm     = [" "++show diaActual]:fechasRecursiva pd lm (diaActual+1) (r-1) 
  | diaActual>lm && r>1                         = ["   "]:fechasRecursiva pd lm diaActual (r-1) 
  | otherwise                                   = [["   "]]

-- otras funciones que se necesiten:

--------------------------------------------------------------------

esBisiesto:: Int -> Bool
esBisiesto a= mod a 4==0 || (mod a 100==0 && mod a 400==0) 