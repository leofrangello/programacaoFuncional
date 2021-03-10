-- exercicio 2 (dobro de um numero)
dobro x = x * 2

-- exercicio 3 (quadruplo de um numero usando a funcao dobro)
quadruplo x = dobro(x) * 2

-- exercicio 4 (dado os catetos retornar a hipotenusa)
hipotenusa x y = sqrt(((x*x)+(y*y)))

-- exercicio 5 (calcular distancia entre dois pontos no plano cartesiano)
type Ponto = (Float, Float)
distancia :: Ponto -> Ponto -> Float 
distancia (x1,y1) (x2,y2) =  sqrt(x*x + y*y)
   where 
      x= x1-x2
      y = y1 - y2
