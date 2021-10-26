
-- **** Ejercicios sencillos de ENTRADA / SALIDA: ****

-- 1) Define un programa pideCadenas :: IO() que pida dos cadenas (o strings) al usuario y responda si son
-- iguales o no.
-- pideCadenas :: IO()

-- 2) Abstraer del programa anterior el subprograma que imprime un mensaje y lee la respuesta. Utiliza
-- dicho subprograma dentro de pideCadenas'
-- escribeylee :: String -> IO String
-- pideCadenas':: IO()

-- 3) Similar al programa (y subprograma) del ejercicio anterior pero leyendo dos enteros y devolviendo su
-- suma, si son iguales, o su producto, si son distintos.
-- escribeyleenum :: String -> IO Int
-- pideNumeros :: IO()

-- 4) Similar al ejercicio anterior cuando los dos números son iguales pero si son distintos vuelve a pedir
-- dos números (hasta leer dos iguales).
-- pideNumeros':: IO() 

-- 5) El programa pide un número (clave) entre 1 y 30 al jugador A y repetidamente le pide al jugador B
-- que lo adivine. El jugador B contesta en cada iteración con un número y el sistema le ayuda diciéndole si
-- dicho número es mayor o menor que el número clave. El programa termina cuando B acierta la clave.
-- juego :: IO ()
-- juego = do
--  clave <- escribeyleenum "Dame un numero clave entre 1 y 30: "
--  putStrLn ( concat ["\n" | _ <- [1..50]] )
--  adivina clave
-- adivina :: Int -> IO()

-- 6) Cambiar el juego anterior de forma que se muestre en qué número de intentos ha conseguido B
-- adivinar el número clave de A.
-- juego' :: IO ()
-- adivina':: Int -> Int -> IO()