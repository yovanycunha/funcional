{-
- Encontra o penultimo elemento de uma lista. Caso a lista seja vazia ou tenha apenas um elemento retorne o seguinte comando: error "Lista sem penultimo" 
-}
penultimo [] = error "Lista sem penultimo"
penultimo [x] = error "Lista sem penultimo"
penultimo xs = last (init xs)

{-
- Retorna o k-esimo (k varia de 0 ate N-1) elemento de uma lista. Ex: elementAt 2 [4,7,1,9] = 7
-}
elementAt 0 xs = head xs
elementAt i xs = elementAt (i-1) (tail xs) 

{-
- Diz se uma lista é palindrome. 
-}
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs 
 | (head xs == last xs) = isPalindrome (init (tail xs))
 | otherwise = False
{-
- Remove os elementos duplicados de uma lista. Ex: compress [2,5,8,2,1,8] = [2,5,8,1]
- Voce pode usar a funcao elem de Haskell
-}
compress [] = []
compress xs = (compress (init xs)) ++ [y | y <- [last xs], not (elem y (init xs))]

{-
- Varre a lista da esquerda para a direita e junta os elementos iguais. Ex: compact [2,5,8,2,1,8] = [2,2,5,8,8,1]
- Voce pode usar funcoes sobre listas como : (cons), filter, etc.
-}
compact [] = []
compact xs = (filter (== (head xs)) xs) ++ compact (filter (/= head xs) (tail xs))

removeList e [] = []
removeList e (x:xs)
 | e == x = removeList e xs
 | otherwise = x:(removeList e xs) 


{-
- Retorna uma lista de pares com os elementos e suas quantidades. Ex: encode [2,2,2,3,4,2,5,2,4,5] = [(2,5),(3,1),(4,2),(5,2)]
- Voce pode usar funcoes sobre listas como : (cons), filter, etc.
-}
encode [] = []
encode xs = (head (ys),length ys) : encode (removeList (head xs) (tail xs))
  where
    ys = filter (== (head xs)) xs

{-
- Divide uma lista em duas sublistas onde o ponto de divisao é dado. Ex: split [3,6,1,9,4] 3 = [[3,6,1],[9,4]]
-}
split xs i = [take i xs] ++ [drop i xs]

{-
- Extrai um pedaço (slice) de uma lista especificado por um intervalo. 
- Ex: slice [3,6,1,9,4] 2 4 = [6,1,9]
-}
slice xs imin imax = drop (imin - 1) (take imax xs)
-- slice imin imax = (drop (imin - 1)).(take imax) 

{-
- Insere um elemento em uma posicao especifica de uma lista. 
- Ex: insertAt 7 4 [3,6,1,9,4] = [3,6,1,7,9,4]
-}
insertAt el pos xs = (take (pos-1) xs) ++ [el] ++ drop (pos-1) xs

{-
- Ordena uma lista em ordem crescente. Voce deve seguir a ideia do selectionsort onde os elementos 
- menores sao trazidos para o inicio da lista um a um. Esta funcao ja esta implementada.
-}
minList [x] = x
minList (x:xs) = if (x < (minList xs)) then x else minList xs

remove e (x:xs) | e == x = xs
                | otherwise = x:(remove e xs)
sort [] = []
sort xs = x:ys 
    where
        x = minList xs
        ys = sort (remove x xs) 

{-
- Dada a funcao max que retorna o maximo entre dois numeros, escreva uma funcao que usa a função
- foldr e max para retornar o maximo de uma lista se a lista não é vazia.
-}
maxList [] = undefined
maxList xs = foldr max (head xs) xs

{-
- Transforma uma string em uma palindrome acrescentando o reverso da string ao seu final sem usar a funcao reverse. 
- Ex: buildPalindrome [1,2,3] = [1,2,3,3,2,1]. 
-}
buildPalindrome [] = []
buildPalindrome (x:xs) = [x] ++ buildPalindrome xs ++[x]

{-
- Computa a media dos elementos de uma lista de numeros, sem usar nenhuma funcao pronta de listas.
-}
mean xs = (fromIntegral (sum xs))/(fromIntegral (length xs))

{-
- Escreva a funcao myAppend que faz o append de uma lista xs com a lista ys, usando a função foldr. 
-}
myAppend xs ys = foldr (:) ys xs

{- 
- Escreva a funcao find, que retorna a primeira ocorrencai de uma lista que satisfaz determinado predicado
-}
find p [] = error "No element found"
find p (x:xs) | p x = x
              | otherwise = find p xs

{- 
- Implementar quick sort usando compreensao de listas
-}
quickSort [] = []
quickSort (x:xs) = (quickSort smaller) ++ [x] ++ (quickSort greater) 
    where
      smaller = [y | y <- xs, y <= x]
      greater = [y | y <- xs, y > x]

{- 
- Implementar quick sort usando filter
-}
quickSortFilter [] = []
quickSortFilter (x:xs) = (quickSortFilter smaller) ++ [x] ++ (quickSortFilter greater)
    where 
      smaller = filter (<=x) xs
      greater = filter (>x) xs

{-
- Implemente uma funcao que soma todos os elementos de uma lista usando foldl
-}
sumFoldl xs = foldl (+) 0 xs

{-
- Retorna uma lista sem repeticao a partir de uma lista possivelmente com elementos repetidos
-}
unique [] = []
unique (x:xs) = x : unique (filter (/= x) xs)

{-
- Conta a quantidade de ocorrencias de um elemento em uma lista
-}
count x [] = 0
count x xs = length (filter (== x) xs)

{-
- Dada uma lista de elementos possivelmente repetidos, retorna uma lista de 
- pares contendo um elemento da lista odiginal e sua quantidade de ocorrencias
- Cada elemento da lista original possui apenas um par correspondente na lista de resposta
-}
frequencia [] = []
frequencia xs =  zip elems counts where
  elems = unique xs
  counts = [ count x xs | x <- elems]

{-
- Funcao concat definida usando foldr
- A funcao concat representa a concatenacao distribuida de listas. Ou seja, recebe uma lista de 
- listas e realiza a concatenacao de todas as listas. 
- Ex: concat [[1,2],[3,4]] = [1,2,3,4]
-}
concatFoldr xs = foldr (++) [] xs

{-
- Funcao que gera todas as tuplas combinando 3 numeros inteiros de 1 até 10
-}
todasTriplas = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10]]

-- valida a condicao basica para uma tripla representar um triangulo, 
-- onde c é sempre considerado o maior lado
triangulo (a,b,c) = c < (a + b)

-- funcao que pega os lados de um triangulo e diz se corresponde a um triangulo retangulo
trianguloRetangulo (a,b,c) = (a^2 + b^2) == c^2

-- funcao que soma os elementos de uma tripla
sumTriple (a,b,c) = a + b + c

-- funcao que verifica se o perimetro de uma tripla é igual a p
perimetro p =  (== p).sumTriple

{-
- Funcao que recebe uma lista de triplas representando os lados de um triangulo e seleciona 
- todos os triangulos retangulos cujo perimetro é igual a um perimetro dado p
-}
triangulosRetangulos p = (filter (perimetro p)).(filter trianguloRetangulo).(filter triangulo)