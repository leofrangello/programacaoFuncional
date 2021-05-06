-- Exercicio 1

paridade::Integral a => [a] -> [Bool]
paridade lst = map (even) lst

-- Exercicio 2
prefixos:: [[a]] -> [[a]]
prefixos lst = map (take 3) lst

-- Exercicio 3
saudacao:: [[Char]] -> [[Char]]
saudacao lst = map ("Oi " ++ ) lst

--  Exercicio 4
filtrar:: (a -> Bool) -> [a] -> [a]
filtrar p [] = []
filtrar p (x:xs)
 |p x = x: filtrar p xs
 |otherwise = filtrar p xs

filtrarCompressao:: (a -> Bool) -> [a] -> [a]
filtrarCompressao cond lst = [z | z<- lst, cond z]

-- Exercicio 5

pares:: Integral a => [a] -> [a]
pares lst = filter (even) lst

-- Exercicio 6

solucoes:: (Ord a, Num a) => [a] -> [a]
solucoes lst = filter (\x -> ((5 * x + 6) < (x * x))) lst

-- Exercicio 7
maior:: (Foldable l, Ord a) => l a -> a
maior lst = foldr1 max lst

-- Exercicio 8
menor_min10:: (Foldable t, Ord b, Num b) => t b -> b
menor_min10 lst = foldr (min) 10 lst

-- Exercicio 9
junta_silabas_plural :: [String] -> String
junta_silabas_plural lst = foldr (++) "s" lst

-- Exercicio 10
menores10:: [Int] -> ([Int], Int)
menores10 lst = let x = filter (<10) lst in (x, length(x))

-- Exercicio 11
procurar:: Eq a => a -> [a] -> Int
procurar a [] = 0
procurar a (x:xs)
 | a == x = 1
 | otherwise = 1 + (procurar a xs)

busca:: Eq a => a -> [a] -> (Bool, Int)
busca x lst = (elem x lst, procurar True (map (x ==) lst))