
module Pila1 (Pila, pvacia )
--Constructoras
--Pila a ==> Vac | Ap a (Pila a)
--Pila a ==> P
--
--Operaciones:    pVacia:: Pila abs
--                apilar:: a -> Pila a -> Pila
--                desapilar:: Pila a -> a
--
--cima::Pila a -> a
--esVacia:: Pila a -> Bool

data Pila a = Vac |Ap a (Pila a)

pVacia :: Pila a
pVacia= Vac

apilar :: a -> Pila a -> Pila a
apilar x p= Ap x p

desapilar :: Pila a -> Pila a
desapilar (Ap _ p) = p
desapilar Vac= error("La pila esta vacía")

cima :: Pila a -> a
cima(Ap x _)=x
cima Vac= error ("La pila esta vacía")

esVacia :: Pila a -> Bool
esVacia Vac = True 
esVacia(Ap _ _)=False 
--
--pVacia :: [a] -> Bool
--pVacia []= True
--pVacia m=False
--
--apilar:: a -> Pila1 a -> Pila1
--apilar a= a++P[a]