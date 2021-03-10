-- exercicio 2 (dobro de um numero)
dobro x = x * 2

-- exercicio 3 (quadruplo de um numero usando a funcao dobro)
quadruplo x = dobro(x) * 2

-- exercicio 4 (dado os catetos retornar a hipotenusa)
hipotenusa x y = sqrt(((x*x)+(y*y)))

-- exercicio 5 (calcular distancia entre dois pontos no plano cartesiano)
distancia (x1,y1) (x2,y2) = distanciaAux x1 y1 x2 y2
distanciaAux x1 y1 x2 y2 = sqrt((x1 - x2)^2 + (y1 - y2)^2)
