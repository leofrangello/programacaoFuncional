-- Exercicio 1
conta::[t]->Int
conta [] = 0
conta (x:r) = 1 +  conta r

conta_ch:: [Char]->Int
conta_ch [] = 0
conta_ch (x:resto) = 1 + conta_ch resto

maior:: [Int]->Int
maior [x] = x
maior (x:y:resto)
 | x > y = maior (x:resto)
 | otherwise = maior (y:resto)

primeiros:: Int->[t]-> [t]
primeiros 0 _ = []
primeiros _ [] = []
primeiros n (x:xs) = x: primeiros (n-1) xs

-- Implementaçao igual a aula, porem esta dando erro
--pertence:: t->[t]->Bool
--pertence a [] = False
--pertence a (x:z) =
-- if (a==x)
-- then True
-- else pertence a z

--uniaoR:: [t]->[t]->[t]
--uniaoR [] l = l
--uniaoR (x:xs) l =
 --if pertence x l
 --then uniaoR xs l
 --else x: uniaoR xs l

-- Exercicio 2
pares::[Int]->[Int]
pares xs = [x | x<-xs, even x]

npares::[Int]->Int
npares x = length(pares x)

-- Exercicio 3
produtorio::[Int]->Int
produtorio []= 0
produtorio [x] = x
produtorio (x:xs) = x*(produtorio xs)

-- Exercicio 4
comprime::[[a]]->[a]
comprime [] = []
comprime (x:xs) = x ++ comprime xs

-- Exercicio 5
contador::[a]->Int->Int
contador [] y = y
contador (x:xs) y = contador xs y+1

tamanho::[a]->Int
tamanho x = contador x 0

-- Exercicio 6
uniaoRec2::Eq a =>[a]->[a]->[a]
uniaoRec2 [] y = y
uniaoRec2 x [] = x
uniaoRec2 x (y:ys) = 
 if (elem y x) == True 
 then uniaoRec2 x ys 
 else uniaoRec2 (x++[y]) ys