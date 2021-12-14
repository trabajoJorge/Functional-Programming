import Data.Char ( isAlpha )

agregar::Int -> Int -> Int
agregar x y=    if y>=0 || y<=9 then 
                    x*10+y
                else 
                error "El segundo parametro no es un digito"


e5_Sumcuad :: Int->Int->Int->Int
e5_Sumcuad x y z
    | x>z && y>z    =x^2+y^2
    | x>y && z>y    =x^2+z^2
    | y>x && z>x    =y^2+z^2
    | otherwise =0

divMad::(Int,Int)->(Int,Int)
divMad (x, y) = (a, b)
                where 
                        a= div x y 
                        b=mod x y

sigLetra::Char->Char
sigLetra x
    | not (isAlpha x) = error"No es un caracter" {- isAlpha para ver si el caracter es alfanumerico-}
    | x=='z' ='a'
    | x=='Z' ='A'
    | otherwise  =toEnum(fromEnum x+1)

digitoVal::Char->Int
digitoVal x= fromEnum x

producto::Int->Int->Int
producto n m 
    | n<m   =product[n..m]
    | n>m   =product[m..n] 
    | n==m  =n*m
    |otherwise =0

edad::(Int, Int, Int)->(Int, Int, Int)->(Int, Int, Int)
edad (d1, m1, a1)  (d2, m2, a2) 
    | m1<=9 && d1<14 = (d2-d1, m2-m1, a2-a1)
    | otherwise  = (d2-d1, m2-m1, a2-a1-1)

xorPersonal::Bool->Bool->Bool 
xorPersonal x y 
    | x==y          =False 
    | otherwise     =True     

tresIgual :: Int -> Int -> Int -> Bool 
tresIgual x y z = a == b
                    where   a= x==y 
                            b= y==z

hms::Int->(Int,Int,Int)
hms i = (hs, ms, s)        
    where   hs= div i 3600
            ms= div i 60 -60*hs
            s= i - (3600*hs) - (60*ms)

triangulo::(Int,Int,Int)->String --Coregir
triangulo (x, y, z)
    | (x<=y && y<=z) || (x>0 && y>0 && z>0)     ="ERROR: No es triangulo"
    | x==y && y==z                              ="TRIANGULO EQUILATERO"
    | x/=y && y/=z && x/=z                      ="TRIANGULO ESALENO"
    | otherwise                                 ="TRIANGULO ISOSCELES"

miAND::Bool->Bool->Bool 
miAND x y 
    | x==y          =True
    | otherwise     =False 

miOR::Bool->Bool->Bool
miOR x y 
    | not x         =False
    | y             =True 
    | otherwise     =True 

-- ** 15 CON VARIABLE ANONIMA **
miOR1::Bool->Bool->Bool
miOR1 _ True= True  -- _ -> variable anonima 
miOR1 x False= x

miAND1::Bool->Bool->Bool 
miAND1 _ False= False
miAND1 a True= a
