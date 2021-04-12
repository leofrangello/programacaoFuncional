--Leonardo Frangello Franzese 11721BCC029

-- Trabalho 1 - Programaçao Funcional

-- Exercicio 1
quadrado:: Int->Int
quadrado a = a*a

analisa_raizes::Int -> Int -> Int -> String
analisa_raizes a b c 
 |a == 0 = "4 - equacao degenerada"
 |quadrado(b) > (4*a*c) = "1 - possui duas raizes reais" 
 |quadrado(b) == (4 * a *c) = "2 - possui uma raiz real"
 |quadrado(b) < (4 * a * c) = "3 - nenhuma raiz real"
 |otherwise = "nao existe funcao"

--Exercicio 2
equacao::Double->Double->Double->(Double,Double)
equacao a b c 
 | a /= 0 = ((-b+sqrt(b*b-4*a*c))/2*a,(-b-sqrt(b*b-4*a*c))/2*a)
 | otherwise = ((-c)/b, a)

 --Exercicio 3
type Data = (Int,Int,Int)

bissexto::Int->Bool
bissexto x = 
 if (mod x 4 == 0 && mod x 100 /= 0 || mod x 400 == 0) 
 then True 
 else False

valida::Data->Bool
valida (d,m,a)
  | d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 || m == 7 ||  m == 8 || m == 10 || m == 12) = True
  | d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 || m == 11) = True
  | d <= 29 && d >= 1 && m == 2 && (bissexto a == True) = True
  | d <= 28 && d >= 1 && m == 2 = True
  | otherwise = False

valorPassagem::Float->Data->Data->Float
valorPassagem preco (d1,m1,a1) (d2,m2,a2) -- Primeira data (atual) segunda data (nascimento)
 | valida(d1,m1,a1) == False || valida(d2,m2,a2) == False = 0
 | a1 - a2 < 2 = preco*0.15
 | a1 - a2 == 2 && m2 > m1 = preco*0.15
 | a1 - a2 == 2 && m2 == m1 && d2>d1 = preco*0.15
 | a1 - a2 < 10 = preco * 0.40
 | a1 - a2 == 10 && m2 > m1 = preco*0.40
 | a1 - a2 == 10 && m2 == m1 && d2 > d1 = preco*0.40
 | a1 - a2 > 70 = preco/2
 | a1 - a2 == 70 && m2 > m1 = preco/2
 | a1 - a2 == 70 && m2 == m1 && d2<=d1 = preco/2
 | otherwise = preco

 --Exercicio 4
gera1 = [n*n*n | n <- [3..11], mod n 2 == 0]

gera2 = [(x,y)| x <- [1..5], y<-[3,6..15]]

gera3 = [n | x <- [15,16], n<- [1..x]]

gera4 = [(x, x+1) | x <- [1..10], even x ]

gera5 = [a+b | (a,b) <- [(x,x+1) | x <- [1..10], even x]]



--Exercicio 5
-- A 
contaNegM2::[Int]->Int
contaNegM2 x = length [y | y <- x, y > 0, mod y 3 == 0]

-- B
listaNegM2::[Int]->[Int]
listaNegM2 x = [y | y <- x, y > 0, mod y 3 == 0]

-- Exercicio 6
fatores::Int->[Int]
fatores n = [x | x <- [1..n], mod n x == 0]

primos::Int->Int->[Int]
primos x y = [r | r <-[x..y], length(fatores r) == 2] -- length == 2 serve que o valor so seja mandado para r caso o tamanho seja 2 (que seria se ele fosse divido por um e por ele mesmo)

-- Exercicio 7
mdc:: Int->Int->Int
mdc a b
 |b == 0 = a
 | otherwise = mdc b (mod a b)

mmc2::Int->Int->Int
mmc2 a b
 |a == b = a
 | otherwise = div (a*b) (mdc a b)

mmc::Int->Int->Int->Int
mmc a b c = mmc2 a (mmc2 b c)

-- Exercicio 8

calculaSerie:: Float -> Int -> Float
calculaSerie x n
 | x == 0 || n == 0 = 0
 | mod n 2 == 0 = x/fromIntegral(n) + calculaSerie x (n-1)
 | mod n 2 /= 0 = fromIntegral(n)/x + calculaSerie x (n-1)


-- Exercicio 9
reversefizzbuzz::Int->[String]
reversefizzbuzz n
 |n==0 = []
 |mod n 2 == 0 && mod n 3 == 0 = "FizzBuzz":reversefizzbuzz(n-1)
 |mod n 2 == 0 = "Fizz":reversefizzbuzz(n-1)
 |mod n 3 == 0 = "Buzz": reversefizzbuzz (n-1)
 | otherwise = "No":reversefizzbuzz(n-1)

fizzbuzz::Int->[String]
fizzbuzz x = reverse (reversefizzbuzz x)

-- Exercicio 10
sel_multiplos::Int->[Int]->[Int]
sel_multiplos n lst = [ r | r <- lst, mod r n == 0]  -- r tal que r é dado pela lista de forma que o resto da divisao do r com o n é zero

-- Exercicio 11
contagem::Int->[Int]->Int
contagem x lst 
 | length lst == 0 = 0
 | head lst == x = 1 + contagem x (tail lst)
 | otherwise = contagem x (tail lst)

unica_ocorrencia::Int->[Int]->Bool
unica_ocorrencia x lst
 | (contagem x lst) > 1 || (contagem x lst) < 1 = False
 | otherwise = True


-- Exercicio 12
intercala::[Int]->[Int]->[Int]
intercala x y
 |x == [] && y == [] = []
 |x == [] = y
 |y == [] = x
 |otherwise = [head x]++[head y]++(intercala (tail x) (tail y))

-- Exercicio 13
zipar::[Int]->[Int]->[[Int]]
zipar a b
 |a == [] && b == [] = []
 |a == [] = []
 |b == [] = []
 |otherwise = [[head a, head b]]++(zipar (tail a) (tail b))
 
-- Exercicio 14
type Contato = (String,String,String,String)

contatos::[Contato]
contatos = [("Leonardo"," Rua Eloy Fernandes","13123456789","leofrangello1@gmailcom"), 
 ("Camila", "Rua Barao de Penedo", "13987654321", "camilakelessis@outlook.com")]

procuraContatoAux::[Contato]->String->String
procuraContatoAux [] email = "Esse desconhecido"
procuraContatoAux ((x,y,z,w):xs) email
 | w == email = x
 | otherwise = procuraContatoAux xs email

achaContato::String->String
achaContato email = procuraContatoAux contatos email

-- Exercicio 15
type Pessoa = (String,  Float, Int, Char)
pessoas :: [Pessoa]
pessoas = [ ("Rosa", 1.66, 27, 'C'),
 ("João", 1.85, 26, 'C'),
 ("Maria", 1.55, 62, 'S'),
 ("Jose", 1.78, 42, 'C'),
 ("Paulo", 1.93, 25, 'S'),
 ("Clara", 1.70, 33, 'C'),
 ("Bob", 1.45, 21, 'C'),
 ("Rosana", 1.58, 39, 'S'),
 ("Daniel", 1.74, 72, 'S'),
 ("Jocileide", 1.69, 18, 'S') ]

-- A 
media lst = (soma_altura lst)/ fromIntegral (length lst) :: Float
soma_altura::[Pessoa]->Float
soma_altura [] = 0.0
soma_altura ((nome,altura,idade,estado):xs) = altura + (soma_altura xs)

-- B
mais_novo::[Pessoa]->Int->Int
mais_novo [] y = y
mais_novo ((nome,altura,idade,estado):xs) y =
 if idade < y 
 then mais_novo xs idade
 else mais_novo xs y

menor_idade::[Pessoa]->Int
menor_idade lst = (mais_novo lst 999) --numero alto para iniciar a lista de mais novo

-- C

mais_velho::[Pessoa]->Int->Int
mais_velho [] y = y
mais_velho ((nome,altura,idade,estado):xs) y =
 if idade > y
 then mais_velho xs idade
 else mais_velho xs y

maior_idade::[Pessoa]->Int
maior_idade lst = (mais_velho lst 0) -- numero baixo pois sempre tera alguem mais vellho que 0, se esse nro fosse 73 retornaria vazio

procuraMaisVelhoAux::[Pessoa]->Int->(String,Char)
procuraMaisVelhoAux [] i = ("Vazio", 'V')
procuraMaisVelhoAux ((x,y,z,w):xs) i
 | z == i = (x,w)
 | otherwise = procuraMaisVelhoAux xs i

procuraMaisVelho::[Pessoa]->(String,Char)
procuraMaisVelho lst = procuraMaisVelhoAux lst (maior_idade pessoas)


-- D 

maior50anos::[Pessoa]->[Pessoa]->[Pessoa]
maior50anos [] _ = []
maior50anos ((nome, altura, idade, estado):xs) y = 
 if idade >= 50
 then maior50anos xs y++[(nome, altura, idade,estado)]
 else maior50anos xs y

imprimeDados::[Pessoa]->[Pessoa]
imprimeDados lst = maior50anos lst []

-- E

procuraCasada::[Pessoa]->Int->[Pessoa]->[Pessoa]
procuraCasada [] _ _ = []
procuraCasada ((nome, altura, idade, estado):xs) i y =
 if (idade>=i) && (estado == 'C')
 then procuraCasada xs i y++[(nome, altura, idade,estado)]
 else procuraCasada xs i y


casados::[Pessoa]->Int->[Pessoa]
casados lst i = procuraCasada lst i []


-- Exercicio 16
insere_ord::Ord a=>a->[a]->[a] --Ord a significa que a funçao funciona para qualquer tipo a desde que a sejja comparavel
insere_ord x [] = [x]
insere_ord x (y:ys)
 | x <= y = (x:y:ys)
 | otherwise = y:(insere_ord x ys)

-- Exercicio 17
reverte::[a]->[a]
reverte lst
 | null lst == True = []
 | otherwise = (reverte (tail lst))++[(head lst)]

--Exercicio 18
auxiliar::Eq a =>([a],a)-> Bool
auxiliar ([], _) = False
auxiliar ((x:xs), y) = 
 if x == y
 then True
 else auxiliar (xs,y)

outra_lista::Eq a => [a]->[a]->[a]
outra_lista [] y = y
outra_lista (x:xs) y = 
 if auxiliar (y, x) == False
 then outra_lista xs (x:y)
 else outra_lista xs y

elimina_repet::Eq a=> [a]->[a]
elimina_repet lst
 |null lst == True = []
 | otherwise = reverse (outra_lista lst [])


-- Exercicio 19
notasDisponiveis::[Int]
notasDisponiveis = [1,2,5,10,20,50,100]

notasTroco::Int->[[Int]]
notasTroco 0 = [[]]
notasTroco valor = [x:xs | x <- notasDisponiveis, valor >= x, xs <- notasTroco (valor-x)]

-- Exercicio 20