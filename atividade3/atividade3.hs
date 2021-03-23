-- ex 1A
ex1A1:: Bool -> Bool -> Bool
ex1A1 True True = True
ex1A1 True False = True
ex1A1 False True = True
ex1A1 False False = False

ex1A2:: Bool -> Bool -> Bool
ex1A2 False False = False
ex1A2 True _ = True

ex1A3:: Bool -> Bool -> Bool
ex1A3 False b = b
ex1A3 True _ = True


-- ex 1B

ex1B1:: Bool -> Bool -> Bool
ex1B1 x y  =
 if (x == False && y == False)
 then False
 else True

ex1B2:: Bool -> Bool -> Bool
ex1B2 x y =
 if (x /= y)
 then True
 else 
 if (x == y && y == False)
  then False
  else True

-- Ex 2
exponencial:: Float -> Float
exponencial x = x * x

dist:: (Float,Float) -> (Float, Float) -> Float
dist (x1,y1) (x2,y2) = sqrt (exponencial (x2-x1) + exponencial (y2-y1))

-- Ex 3
fatorial:: Int->Int
fatorial 0 = 1
fatorial n = n * fatorial (n-1)

fatorial2:: Int->Int
fatorial2 x = if(x==0) then 1
else (x*fatorial2(x-1))

-- eX 4
fibog:: Int->Int
fibog n
 | n == 0 = 1
 | n == 1 = 1
 | otherwise = (fibog (n-2) + fibog (n-1))

 
 -- ex 5
n_tri:: Int ->Int 
n_tri n
 | n == 0 = 0
 | n == 1 = 1
 | n == 2 = 3
 | n == 3 = 6 
 |otherwise = n_tri(n-3) + n_tri(n-2) + n_tri (n-1)

 -- Ex 6
potencia2:: Int ->Int
potencia2 expoente
 | expoente == 0 = 1
 | otherwise = (2 * potencia2(expoente-1))


-- ex7
prodIntervalo::Int->Int->Int
prodIntervalo m n
 | m == n = n
 | otherwise = n * prodIntervalo m(n-1)

-- ex 7 pt 2
fibProdIntervalo:: Int->Int
fibProdIntervalo n
 | 1 == n = n
 | otherwise = n * fibProdIntervalo (n-1)

-- ex 8
resto_div:: Int->Int->Int
resto_div dividendo divisor = 
 if dividendo == 0 || divisor == 1
 then 0
 else 
 if dividendo < divisor
  then dividendo
  else resto_div (dividendo - divisor) divisor

-- ex 9
-- Utilizando guarda
mdcGuarda::(Int, Int) -> Int
mdcGuarda (m,n)
 | n == 0 = m
 | otherwise = mdcGuarda (n, (mod m n))

-- Casamento de padrao
mdccasamento:: (Int, Int)->Int
mdccasamento (m, 0) = m
mdccasamento (m, n) = mdccasamento (n, (mod m n))


-- ex 10
-- Guarda
binomialGuarda::(Int, Int)->Int
binomialGuarda (n, k)
 | k == 0 = 1
 | k == n = 1
 | otherwise = binomialGuarda (n-1, k) + binomialGuarda (n-1, k-1)

-- Casamento de padrao
binomialPadrao::(Int, Int)->Int
binomialPadrao(n,0) = 1
binomialPadrao (n, k) =
 if (k==n)
  then 1
  else binomialPadrao (n-1, k) + binomialPadrao (n-1, k-1)


-- Exercicio 11
passo::(Int, Int)-> (Int,Int)
passo (x,y) = (y, x+y)

auxFibo::Int->(Int,Int)
auxFibo 1 = (1,1)
auxFibo n = passo (auxFibo (n-1))

fibo2::Int->Int
fibo2 n = do 
   let (x,y) = auxFibo n
   x