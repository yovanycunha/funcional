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
        
myTake k xs = undefined -- verifique se sua solucao produz o mesmo resultado que "take 5 [1..]"
myDrop k xs = undefined
myMaximum xs = undefined
myMinimum xs = undefined
mySum xs = undefined
myProduct xs = undefined
myElem xs = undefined
myRange k m = undefined -- sua versao da funcao [k..m]. existem as funcoes succ e pred (sucessor e predecessor que podem ser uteis)
myRangeStep k p m = undefined -- sua versao da funcao [k,p..m]
myCycle xs = undefined
myRepeat n = undefined
myReplicate k n = undefined
