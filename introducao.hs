{-
- Usando os predicados not,and e or prontos de Haskell, implemente os predicados (funcoes) xor (or exclusivo),
- impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
- Procure usar casamento de padroes e reutilizar as funcoes.
-}

xor False True = True
xor True False = True
xor _ _ = False

impl a b = not a || b
equiv a b =  impl a b && impl b a

{-
A funcao square esta implementada e eleva ao quadrado um determinado numero
-}
square x = x*x

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-}
pow x 0 = 1
pow x y
    | y > 0 = x * pow x (y-1)
    | otherwise = 1 / pow x (-y)

{-
- Implemente a funcao fatorial que calcula o fatorial de um numero 
-}
fatorial 0 = 1
fatorial x = x * fatorial (x-1)

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}
isPrime x 
        | x >= 1 && 3 >= x = True
        | otherwise = isPrimeWithList x [2..(x-1)]
        
isPrimeWithList x [] = False
isPrimeWithList x (y:ys) 
                | mod x y == 0 = False
                | otherwise = isPrimeWithList x ys

{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}
fib x 
    | x == 1 || x == 2 = 1
    | otherwise = fib (x-1) + fib (x-2)

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
-}
mdc x y 
    | mod x y == 0 = y
    | otherwise = mdc y (mod x y)

{-
- Calcula um MMC de dois numeros. 
-}
mmc x y = undefined

{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-}
coprimo x y
        | mdc x y == 1 = True
        | otherwise = False

{- FUNCOES SOBRE LISTAS -}
{-
- Implemente suas versoes para as funcoes abaixo sobre listas "myFunc", seguindo a mesma semantica da funcao original "func"
- Use pattern matching sempre que possivel ou entao guardas. 
- Voce nao pode utilizar as respectivas funcoes originais mas implementar sua própria logica
-}
myLength xs
          | null xs = 0
          | otherwise = myLen 1 xs

myLen acc (x:xs)
      | null xs = acc
      | otherwise = myLen (acc+1) xs 
          
myReverse xs
          | null xs = []
          | otherwise = myRev xs

myRev xs
      | null (tail xs) = xs
      | otherwise = last xs:myRev (init xs)
        
-- verifique se sua solucao produz o mesmo resultado que "take 5 [1..]"
myTake :: Integer -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake k (x:xs) = (x:myTake (k-1) xs)

myDrop :: Integer -> [a] -> [a]
myDrop 0 xs = xs
myDrop _ [] = []
myDrop k (x:xs) = myDrop (k-1) xs

myMaximum :: Ord a => [a] -> a
myMaximum (x:xs)
          | null xs = x
          | x > head xs = myMaximum (x:tail xs)
          | otherwise = myMaximum xs

myMinimum :: Ord a => [a] -> a
myMinimum (x:xs)
          | null xs = x
          | x < head xs = myMinimum (x:tail xs)
          | otherwise = myMinimum xs

mySum :: [Integer] -> Integer
mySum [] = 0
mySum [x] = x
mySum (x:xs) = x + mySum xs

myProduct :: [Integer] -> Integer
myProduct [] = 1
myProduct [x] = x
myProduct (x:xs) = x * myProduct xs

myElem :: Integer -> [Integer] -> Bool
myElem _ [] = False
myElem a (x:xs)
      | a == x = True
      | otherwise = myElem a xs

myRange :: Integer -> Integer -> [Integer]
myRange k m
        | k == m = [k]
        | otherwise = k:myRange (k+1) m

myRangeStep k p m
            |  k > m || k > p = []
            | otherwise = [k] ++ myRangeStep p (p+p-k) m

myCycle xs = undefined
myRepeat n = undefined

myReplicate :: Integer -> Integer -> [Integer]
myReplicate 0 _ = []
myReplicate 1 n = [n]
myReplicate k n = n:myReplicate (k-1) n

{-
- Calcula um termo da sequencia de Fibonnacci usando com apenas uma chamada recursiva.
-}
fibx n = fibx' 0 1 n
fibx' a b count
    | count == 0 = a
    | otherwise = fibx' b (a+b) (count-1)

{-
- Calcula um termo da sequencia de Fibonnacci usando com apenas uma chamada recursiva.
-}
newfib :: Integer -> Integer
newfib n = newfib' 0 1 n
newfib' :: Integer ->  Integer -> Integer -> Integer
newfib' a _ 0 = a
newfib' a b n = newfib' b (a+b) (n-1)
