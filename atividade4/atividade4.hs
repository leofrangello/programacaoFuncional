-- exercicio 3
-- A
geraIntervalo a b =
 if (a > b)
 then []
 else if (a == b)
 then [a]
 else [a..b]

 -- B
verificaPar a =
 if (mod a 2 == 0)
 then True
 else False

geraIntervaloPar a b =
 if (a > b || a == b)
 then []
 else if (verificaPar a == True)
 then tail [a, a+2..b]
 else tail [a, a+3..b]

 -- Exercicio 4
 -- print no repositorio.

-- Exercicio 5
listaQuadrado::Int->Int->[Int]
listaQuadrado a b = [x*x | x<-[a..b]]

-- Exercicio 6
seleciona_Impares::[Int]->[Int]
seleciona_Impares lst = [x | x <- lst, mod x 2 ==1]

-- Exercicio 7
tabuada::Int->[Int]
tabuada a = [a * x| x <- [1..10]]

-- Exercicio 8
bissexto::Int->Bool
bissexto x = if (mod x 4 == 0 && mod x 100 /= 0 || mod x 400 == 0) 
 then True 
 else False

bissextos::[Int]->[Int]
bissextos lst = [x | x <- lst, bissexto x == True]


-- Exercicio 9 
sublistas::[[a]]->[a]
sublistas [] = []
sublistas (x:xs) = x++[y | y <- sublistas xs]

-- Exercicio 10
type Data = (Int,Int,Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

bdEmprestimo::Emprestimos
bdEmprestimo =
 [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
 ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
 ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

valida::Data->Bool
valida (d,m,a)
 | d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 || m == 7 ||
 m == 8 || m == 10 || m == 12) = True
 | d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 || m == 11) = True
 | d <= 29 && d >= 1 && m == 2 && (bissexto a == True) = True
 | d <= 28 && d >= 1 && m == 2 = True
 | otherwise = False

precede::Data->Data->Bool
precede (d1,m1,a1) (d2,m2,a2)
 | (d2 > d1) && (m2 == m1) && (a2 == a1) && (valida (d1,m1,a1) == True) && 
 (valida (d2,m2,a2) == True) = True
 | (m2 > m1) && (a2 == a1) && (valida (d1,m1,a1) == True) && 
 (valida (d2,m2,a2) == True) = True
 | (a2 > a1) && (valida (d1,m1,a1) == True) && (valida (d2,m2,a2) == True) = True
 | otherwise = False

pegaData::Emprestimo->Data
pegaData (_,_,_,(d,m,a),_) = (d,m,a)

atrasados::Emprestimos->Data->Emprestimos
atrasados bdEmprestimo (d,m,a) = [ r | r <- bdEmprestimo, (precede (pegaData r) (d,m,a)) == True]

-- Exercicio 11

uniaoNRec::[Int]->[Int]->[Int]
uniaoNRec x y = x++[r | r <- y, (elem r x) == False]