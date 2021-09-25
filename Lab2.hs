import Data.List (permutations)
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

--Ejercicio 5 * No entiendo lo de xss
sonpermde :: Eq a => [[a]] -> [[a]]
sonpermde (xs:xss) = filter (perm xs) (xs:xss)

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

-- Ejercicio 11 ***


-- Ejercicio 12
permutaciones_DataList:: [a] -> [[a]]
permutaciones_DataList a = permutations a

-- Ejercicio 13 ***

-- Ejercicio 15
--10

--11

--12
permutaciones :: (Eq a) => [a] -> [[a]]
permutaciones [] = [[]]
permutaciones l = [a:x | a <- l, x <- (permutaciones(filter (\x -> x /= a) l))]

-- Ejercicio 16 ***
auxDiag :: [Char] -> [Char]
auxDiag (x:"") =""
auxDiag (x:xs)= [x] ++ "\n " ++ auxDiag " "++xs 

diag :: [Char] -> IO()
diag a= putStr(auxDiag a)

-- Ejercicio 17
repLong :: [Char] -> IO()
repLong s= putStr(auxRep (length s) s)

auxRep :: Int -> [Char] -> [Char]
auxRep 0 _= ""
auxRep x s = s ++ "\n" ++ auxRep (x-1) s