
import Calendarios(printCalendario)

-- NOTA: La letra ñ ha sido sustituida por n. Cualquier palabra malsonante es pura coincidencia.

stringToInt :: String -> Int
stringToInt s= read s :: Int

main :: IO ()
main= do
        print "Introduce el numero de columnas que quieras en tu calendario (3 o 4): "
        c1 <- getLine
        let n1= stringToInt c1  
        if (n1>4 || n1<3) then do
        print ("El numero introduccido no esta dentro del rango")
        main
        else do 
        print "Introduce el ano del calendario que quieras imprimir (1000<=ano<=9999): "
        c2 <- getLine 
        let n2= stringToInt c2
        if (n2<1000 || n2>9999) then do
                print ("El ano no es correcto") 
                printCalendarioUsuario n1
        else do
                printCalendario n1 n2

printCalendarioUsuario :: Int -> IO ()
printCalendarioUsuario col= do
                                print "Introduce el ano del calendario que quieras imprimir (1000<=ano<=9999): "
                                c2 <- getLine 
                                let n2= stringToInt c2
                                if (n2<1000 || n2>9999) then do
                                        print ("El ano no es correcto") 
                                        printCalendarioUsuario col
                                else do
                                        printCalendario col n2