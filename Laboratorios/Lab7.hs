-- **** Ejercicios sencillos de ENTRADA / SALIDA: ****
    
    stringToInt :: String -> Int
    stringToInt s= read s :: Int

-- 1) Define un programa pideCadenas :: IO() que pida dos cadenas (o strings) al usuario y responda si son
-- iguales o no.
    pideCadenas :: IO ()
    pideCadenas=  do 
                    e<-getLine
                    a<-getLine
                    if a== e then print("Son iguales")
                    else print("Son distintas")


-- 2) Abstraer del programa anterior el subprograma que imprime un mensaje y lee la respuesta. Utiliza
-- dicho subprograma dentro de pideCadenas'
    escribeylee :: String -> IO String
    escribeylee m=  do 
                        print m
                        e<-getLine
                        a<-getLine
                        if a== e then return "Son iguales"
                        else return "Son distintas"


-- 3) Similar al programa (y subprograma) del ejercicio anterior pero leyendo dos enteros y devolviendo su
-- suma, si son iguales, o su producto, si son distintos.
    escribeyleenum :: String -> IO Int
    escribeyleenum m=  do 
                        print m
                        a<-getLine
                        b<-getLine
                        if stringToInt a /= stringToInt b then return ( (stringToInt a) * (stringToInt b) )
                        else  return ( (stringToInt a) + (stringToInt b) )


-- 4) Similar al ejercicio anterior cuando los dos números son iguales pero si son distintos vuelve a pedir
-- dos números (hasta leer dos iguales).
    pideNumeros :: IO ()
    pideNumeros =  do 
                        a<- getLine
                        b<-getLine
                        if stringToInt a /= stringToInt b then pideNumeros
                        else print("Programa Finalizado")


-- 5) El programa pide un número (clave) entre 1 y 30 al jugador A y repetidamente le pide al jugador B
-- que lo adivine. El jugador B contesta en cada iteración con un número y el sistema le ayuda diciéndole si
-- dicho número es mayor o menor que el número clave. El programa termina cuando B acierta la clave.
    juego :: IO ()
    juego = do
                print "Dame un numero clave entre 1 y 30: "
                c1 <- getLine
                let n1= stringToInt c1
                if (n1>30 || n1<1) then error("El numero introduccido no esta dentro del rango")
                else do 
                    adivina n1


    adivina :: Int -> IO ()
    adivina n1= do
                    c2 <- getLine
                    let n2 = stringToInt c2
                    if n1 == n2 then print("Felicidades lo has adivinado")
                    else do
                        print("Casi lo tienes, intentalo de nuevo")
                        adivina n1


-- 6) Cambiar el juego anterior de forma que se muestre en qué número de intentos ha conseguido B
-- adivinar el número clave de A.
    juego2 :: IO ()
    juego2 = do
                print "Dame un numero clave entre 1 y 30: "
                c1 <- getLine
                let n1= stringToInt c1
                if (n1>30 || n1<1) then error("El numero introduccido no esta dentro del rango")
                else do 
                    adivinaConIntentos n1 1
    

    adivinaConIntentos :: Int -> Int  -> IO ()
    adivinaConIntentos n1 i = do
                                c2 <- getLine
                                let n2 = stringToInt c2
                                if n1 == n2 then print("Felicidades lo has adivinado. Solo has necesitado "++ show i ++ " intento/s")
                                else do
                                    print("Casi lo tienes, intentalo de nuevo. Llevas "++ show i ++ " intento/s")
                                    adivinaConIntentos n1 (i+1)
