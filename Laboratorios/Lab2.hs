
import Data.Char (digitToInt)


-- Ejercicio 1
quitaUno :: Eq a => a -> [a] -> [a] -- Eq para que la a sea de un tipo generico que se pueda igualar.
quitaUno _ [] = []
quitaUno x (y:ys) | x == y    = quitaUno x ys
                  | otherwise = y : quitaUno x ys

-- Ejercicio 2
quitaRep :: (Eq a) => [a] -> [a]
quitaRep [] = []
quitaRep [x] = [x]
quitaRep (x:xs) = x : [ k | k <- quitaRep xs, k /=x ]

-- Ejercicio 3
dif::Eq a=>[a]->[a]->[a]
dif x [] = x
dif x (y:ys) = dif (quitaUno y x) ys

--Ejercicio 4
perm::Eq a=>[a]->[a]->Bool
perm x y = (dif x y == []) && (dif y x == [])

--Ejercicio 5
sonpermde :: Eq a => [[a]] -> [[a]]
sonpermde (xs:xss) = filter (perm xs) (xs:xss) -- xss es la cola de una lista de listas

-- Ejercicio 6
aDecimal :: Num int => [int] -> int
aDecimal = foldl addDigit 0
   where addDigit num d = 10*num + d

digits :: Int-> [Int]
digits = map digitToInt . show

-- Ejercicio 7
toBin :: Int -> [Int]
toBin 0 = [0]
toBin n =   if n `mod` 2 == 1 then toBin (n `div` 2) ++ [1]
            else toBin (n `div` 2) ++ [0]

decimalAbinario:: Int -> Int
decimalAbinario x= aDecimal (toBin x)

binarioAdecimal :: Int -> Int
binarioAdecimal 0 = 0
binarioAdecimal i = 2 * binarioAdecimal (div i 10) + (mod i 10)

-- Ejercicio 8
ordenada :: Ord a => [a] -> Bool
ordenada (x:xs)=  if (xs/=[])then (x<=head xs) && ordenada xs
                  else True

-- Ejercicio 9
palabra   :: [Char] -> [[Char]]
palabra s =  case dropWhile (==' ') s of
                     "" -> []
                     sn -> w : palabra snn
                        where (w, snn) = break (==' ') sn

-- Ejercicio 10 
posiciones :: Eq b => b -> [b] -> [Int]
posiciones n a = fst (unzip (filter ((==n).snd) (zip[0..(length a)] a)))

-- Ejercicio 11 
paratodo :: (a -> Bool) -> [a] -> Bool
paratodo s l = length l == length (filter s l)

-- Ejercicio 12 
permutaciones :: (Eq a) => [a] -> [[a]]
permutaciones [] = [[]]
permutaciones l = [a:x | a <- l, x <- (permutaciones(filter (\x -> x /= a) l))]

-- Ejercicio 13 
sublista :: Eq a => [a] -> [a] -> Bool
sublista [] l= True
sublista s []= False
sublista s l = if (head s == head l) then
                  sublista(tail s) (tail l)
               else
                  sublista s (tail l)

subSecuencia :: Eq a => [a] -> [a] -> Bool
subSecuencia [] l= True
subSecuencia s []= False
subSecuencia s l = if (head s == head l) then
                  subSecuencia(tail s) (tail l)
               else
                  False   

-- Ejercicio 15
--10 
filtroTuplas e list = [(x,y) | (x,y) <- list, x == e] 
posiciones1 :: Eq alpha=>alpha -> [alpha] -> [Int]
posiciones1 e list = index
                     where 
                        (lista, index) = unzip (filtroTuplas e (zip list [0..]))

--11
paratodo1 :: Eq a => (a -> Bool) -> [a] -> Bool 
paratodo1 s l = length [x | x<-l, s(x)] == length l

--12
permutaciones1 :: (Eq a) => [a] -> [[a]]
permutaciones1 [] = [[]]
permutaciones1 l = [a:x | a <- l, x <- (permutaciones(filter (\x -> x /= a) l))]

-- Ejercicio 16 
auxDiag::[Char]->[Char]->[Char]
auxDiag e [] = []
auxDiag e (x:xs) = ne ++ [x] ++ "\n" ++ (auxDiag ne xs)
                     where ne = e ++ " "

diag::[Char]->IO()
diag [] = putStr("")
diag p = putStr(auxDiag "" p)


-- Ejercicio 17
repLong :: [Char] -> IO()
repLong s= putStr(auxRep (length s) s)

auxRep :: Int -> [Char] -> [Char]
auxRep 0 _= ""
auxRep x s = s ++ "\n" ++ auxRep (x-1) s

eliminarPrimeraLista :: [alpha] -> [alpha]-> [alpha]
eliminarPrimeraLista _ []= []
eliminarPrimeraLista _ y=  y