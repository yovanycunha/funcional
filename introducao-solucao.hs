{-
- Usando os predicados not,and e or prontos de Haskell, implemente os predicados (funcoes) xor (or exclusivo),
- impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
- Procure usar casamento de padroes e reutilizar as funcoes.
-}
xor True False = True
xor False True = True
xor _ _ = False

impl a b = (not a) || b
equiv a b = (impl a b) && (impl b a)

{-
A funcao square esta implementada e eleva ao quadrado um determinado numero
-}
square x = x*x

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-}
pow x 0 = 1
pow x y
  | y > 0 = x*(pow x (y-1))
  | otherwise = 1/(pow x (-y))


{-
- Implemente a funcao fatorial que calcula o fatorial de um numero 
-}
fatorial 0 = 1
fatorial x = x * fatorial (x-1)

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}
isPrime 1 = True
isPrime 2 = True
isPrime 3 = True
isPrime x = isPrime' x [2..(x-1)]

isPrime' x [] = True
isPrime' x (y:ys) = if (mod x y) == 0 then False else isPrime' x ys

{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}
fib x = last (fib' x)

fib' 1 = [1]
fib' 2 = [1,1]
fib' n = previous ++ [(last (init previous)) + (last previous)]
  where
    previous = fib' (n-1)

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
- descricao em https://www.khanacademy.org/computing/computer-science/cryptography/modarithmetic/a/the-euclidean-algorithm
-}
mdc 0 y = y
mdc x 0 = x
mdc x y = mdc y r
  where
    r = mod x y

{-
- Calcula um MMC de dois numeros. 
-}
mmc x y = head ys
  where ys = filter (divisivel x y) [(max x y)..x*y]
divisivel x y n = (mod n x == 0) && (mod n y == 0)

{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-}
coprimo x y = if (mdc x y) == 1 then True else False


{- FUNCOES SOBRE LISTAS -}
{-
- Implemente suas versoes para as funcoes abaixo sobre listas "myFunc", seguindo a mesma semantica da funcao original "func"
- Use pattern matching sempre que possivel ou entao guardas. 
- Voce nao pode utilizar as respectivas funcoes originais mas implementar sua própria logica
-}
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myTake _ [] = []
myTake 0 _ = []
myTake k (x:xs) = x : rest
  where rest = myTake (k-1) xs

myDrop _ [] = []
myDrop 0  xs = xs
myDrop k xs = myDrop (k-1) (tail xs)

myMaximum [x] = x
myMaximum (x:xs) | x > myMaximum xs = x
                 | otherwise = myMaximum xs

myMinimum [x] = x
myMinimum (x:xs) | x < myMinimum xs = x
                 | otherwise = myMinimum xs

mySum [] = 0
mySum (x:xs) = x + mySum xs

myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

myElem _ [] = False 
myElem x (y:ys) | x == y = True
                | otherwise = myElem x ys

myRange k m | k <= m = [k] ++ myRange (succ k) m
            | otherwise = []

myRangeStep k next m | (next >= k && k <= m) || (next <= k && k >= m) = k : myRangeStep next (2*next -k) m 
    | otherwise = []

myCycle [] = []
myCycle xs = xs ++ myCycle xs

myRepeat n = n:myRepeat n

myReplicate 0 _ = []
myReplicate k n = n: myReplicate (k - 1) n
