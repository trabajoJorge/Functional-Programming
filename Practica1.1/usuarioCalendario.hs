import Calendarios(printCalendario)

stringToInt :: String -> Int
stringToInt s= read s :: Int

printCalendarioUsuario :: IO ()
printCalendarioUsuario= do
                            print "Introduce el numero de columnas que quieras en tu calendario (3 o 4): "
                            c1 <- getLine
                            let n1= stringToInt c1
                            if (n1>4 || n1<3) then error("El numero introduccido no esta dentro del rango")
                            else do 
                                    print "Introduce el año del calendario que quieras imprimir: "
                                    c2 <- getLine 
                                    let n2= stringToInt c2
                                    if (n2<1000) then error ("El dibujo no es correcto") --Al no estar corregida la anterior practica no puedo imprimir años anteriores al año 1000
                                    else do
                                            printCalendario n1 n2