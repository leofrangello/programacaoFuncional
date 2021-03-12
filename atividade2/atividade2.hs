-- ex 1
dobro :: Float->Float
dobro x = x * 2

quadruplo :: Float->Float
quadruplo x = dobro(x) * 2

hipotenusa :: Float->Float->Float
hipotenusa x y = sqrt(((x*x)+(y*y)))

distancia :: (Float,Float)->(Float,Float)->Float
distancia (x1,y1) (x2,y2) = distanciaAux x1 y1 x2 y2
distanciaAux :: Float -> Float -> Float ->Float -> Float
distanciaAux x1 y1 x2 y2 = sqrt((x1 - x2)^2 + (y1 - y2)^2)


-- exercicio 2 --- printscreen

-- exercicio 3 -- 	conversao
conversao :: (Float)-> (Float,Float, Float)
conversao x = ((x*1),(x * 3.96), (x*4.45))

-- exercicio 4 -- bissexto

bissexto :: Int -> Bool
bissexto x
   |(mod x 400 == 0) = True
   |(mod x 4 == 0) && (mod x 100 /=  0 ) = True
   |otherwise = False

-- exercicio 5 -- bissexto 2
type Data = (Int,Int,Int)
bissexto2::Data -> Bool
bissexto2 (d,m,a)
   | d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12) && (bissexto (a) == True) = True
   | d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 || m == 11) && (bissexto (a) == True) = True
   | d <= 29 && d >= 1 && m == 2 && (bissexto (a) == True) = True
   | otherwise = False


-- exercicio 6 -- valida
valida :: Data -> Bool
valida (d,m,a)
   | d >= 1 && d<=31 && (m==1 || m==3 || m==5 || m==7 || m==8 || m==10 || m==12) = True
   | d >= 1 && d<=30 && (m==4 || m==6 || m==9 || m==1) = True
   | d >= 1 && d<=28 && m==2 && not (bissexto(a)) = True
   | d >=1 && d<=29 && m==2 && (bissexto(a)) = True
   | otherwise = False

-- exercicio 7 -- precede data
precede :: Data -> Data -> Bool
precede (d1,m1,a1) (d2,m2,a2)
   | (d1 < d2) && (m1 == m2) && (a1 == a2) && (valida(d1,m1,a1) == True) && (valida(d2,m2,a2) == True) = True
   | (m1 < m2) && (a1 == a2)  && (valida(d1,m1,a1) == True) && (valida(d2,m2,a2) == True) = True
   | (a1 < a2)  && (valida(d1,m1,a1) == True) && (valida(d2,m2,a2) == True) = True
   | otherwise = False

-- exercicio 8 -- Estrutura
type Livro = (String, String, String, String, Int)
type Aluno = (String, String, String, Int)
type Emprestimo = (String, String, Data,Data,String)

-- exercicio 9 -- Emprestimo
e1 :: Emprestimo
e1 = ("H123C9","11721BCC029", (12,9,2009), (20,9,2009), "aberto")

checarEmprestimo:: Emprestimo->Data->Bool
checarEmprestimo (_,_,(d1,m1,a1),(d2,m2,a2),_) (d3,m3,a3)
   |(d3 > d1) && (m3 == m1) && (a3 == a1) && (valida (d3,m3,a3) == True) && (valida (d1,m1,a1) == True) && (d2 > d3) && (m2 == m3) && (a2 == a3) && (valida (d3,m3,a3) == True) && (valida (d2,m2,a2) == True) = True
   |(m3 > m1) && (a3 == a1) && (valida (d1,m1,a1) == True) && (valida (d3,m3,a3) == True) && (m2 > m3) && (a2 == a3) && (valida (d2,m2,a2) == True) && (valida (d3,m3,a3) == True) = True
   |(a3 > a1) && (valida (d1,m1,a1) == True) && (valida (d3,m3,a3) == True) && (a3 > a2) && (valida (d2,m2,a2) == True) = True
   |otherwise = False