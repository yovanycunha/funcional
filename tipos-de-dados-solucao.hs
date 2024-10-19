--Escreva um tipo Quadruple que contem 4 elementos: dois de um mesmo tipo e outros dois de outro tipo
--Escreva as funcoes frstTwo e secondTwo que retornam os dois primeiros e os dois ultimos, respectivamente
data Quadruple a b = Quadrupla a a b b

firstTwo (Quadrupla a1 a2 b1 b2) = (a1,a2)
secondTwo (Quadrupla a1 a2 b1 b2)  = (b1,b2)

--Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
--Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nothing se o valor nao existe
data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d

tuple1 (Tuple1 a) = Just a
tuple1 (Tuple2 a b) = Just a
tuple1 (Tuple3 a b c) = Just a
tuple1 (Tuple4 a b c d) = Just a

tuple2 (Tuple2 a b) = Just b
tuple2 (Tuple3 a b c) = Just b
tuple2 (Tuple4 a b c d) = Just b
tuple2 _ = Nothing

tuple3 (Tuple3 a b c) = Just c
tuple3 (Tuple4 a b c d) = Just c
tuple3 _ = Nothing

tuple4 (Tuple4 a b c d) = Just d
tuple4 _ = Nothing

data List a = Nil | Cons a (List a) deriving (Eq,Show)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead Nil = error "Empty list"
listHead (Cons x xs) = x

listTail Nil = error "Empty list"
listTail (Cons x xs) = xs

listFoldr f v Nil = v
listFoldr f v (Cons x xs) = f x (listFoldr f v xs)

listFoldl f v Nil = v
listFoldl f v (Cons x xs) = listFoldl f (f v x) xs 

--Escreva as funcoes sobre a estrutura de dados binary tree
data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a)
 deriving (Eq,Show)

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

--verifica se uma BT é uma BST
isBST NIL = True
isBST (Node a left right) = (isBSTNode a left right) && isBST left && isBST right

isBSTNode a left right = and ([ a > x | x <- menores] ++ [ a < y | y <- maiores]) where
    menores = order left
    maiores = order right

--insere uma nova chave na BST retornando a BST modificada
insert NIL x = Node x NIL NIL
insert (Node a left right) x | x < a = Node a (insert left x) right
                            | x > a = Node a left (insert right x)
                            | otherwise = Node a left right

--retorna o Node da BST contendo o dado procurado ou entao NIL
search NIL _ = NIL
search (Node a left right) x | x == a = Node a left right
                             | x < a = search left x
                             | otherwise = search right x

--retorna o elmento maximo da BST
maximumBST NIL = error "Maximum does not exist"
maximumBST (Node a _ NIL) = a
maximumBST (Node a _ right) = maximumBST right

--retorna o elemento minimo da BST
minimumBST NIL = error "Minimum does not exist"
minimumBST (Node a NIL _) = a
minimumBST (Node a left _) = minimumBST left


removeMaybe (Just x) = x

--retorna o predecessor de um elemento da BST, caso o elemento esteja na BST
predecessorList NIL x = [Nothing]
predecessorList (Node _ NIL NIL) x = [Nothing]
predecessorList (Node a left right) x | (left /= NIL) && (a == x) = [Just (maximumBST left)]
                                      | (left /= NIL) && (x < a) = predecessorList left x
                                      | (left == NIL) && (x == a) = [Nothing]
                                      | (x > a) = [last (filter (/= Nothing) ((Just a):predecessorList right x))]

predecessor bst x  | ((search bst x == NIL) || (filter (/= Nothing) (predecessorList bst x)) == []) = error "Predecessor nao existe"
                   | otherwise = removeMaybe (head (predecessorList bst x))

--retorna o sucessor de um elemento da BST, caso o elemento esteja na BST
successorList NIL x = [Nothing]
sucessorList (Node _ NIL NIL) x = [Nothing]
sucessorList (Node a left right) x | (right /= NIL) && (a == x) = [Just (minimumBST right)]
                                | (right /= NIL) && (x > a) = sucessorList right x
                                | (right == NIL) && (x == a) = [Nothing]
                                | (x < a) = [last (filter (/= Nothing) ((Just a):sucessorList left x))]

sucessor bst x  | ((search bst x == NIL) || (filter (/= Nothing) (sucessorList bst x) == []))  = error "Sucessor nao existe"
                | otherwise = removeMaybe (head (sucessorList bst x))

--remove um lemento da BST
remove x NIL = NIL
remove x (Node a left right) | (a == x) && left == NIL && right == NIL = NIL
                             | (a == x) && left == NIL && right /= NIL = right
                             | (a == x) && left /= NIL && right == NIL = left
                             -- | (a == x) && left /= NIL && right /= NIL = Node (sucessor (Node a left right) x) left (remove (sucessor (Node a left right) x) right)
                             | (a == x) && left /= NIL && right /= NIL = let replacer = sucessor (Node a left right) x in Node replacer left (remove replacer right)
                             | (x < a) = Node a (remove x left) right
                             | otherwise = Node a left (remove x right)

--retorna uma lista com os dados da BST nos diversos tipos de caminhamento
preOrder NIL = []
preOrder (Node a left right) = [a] ++ preOrder left ++ preOrder right

order NIL = []
order (Node a left right) = order left ++ [a] ++ order right

postOrder NIL = []
postOrder (Node a left right) = postOrder left ++ postOrder right ++ [a]

tree1 = NIL
tree2 = insert NIL 15
tree3 = insert tree2 7
tree4 = insert tree3 4
tree5 = insert tree4 10
tree6 = insert tree5 8
tree7 = insert tree6 2