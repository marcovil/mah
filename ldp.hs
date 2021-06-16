primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs , x `mod` p /= 0]

data Point a = Pt a a

x = 5 + 3

-- non potrei fare x = 4, mi darebbe errore di multiple declarations
-- potremmo farlo solo nell'interprete (definisco nuovi ambienti locali)

add = \ x -> (\ y -> x + y)

mysucc = add 1

fibonacci n = fst (fibAux n) where
  fibAux 1 = (0,1)
  fibAux n = (snd previous, fst previous + snd previous)
    where previous = fibAux (n-1)

v = 1/0

addList [] _ = []
addList _ [] = []
addList (x:xs) (y:ys) = (x + y) : addList xs ys

fib = 1:(addList fib (0:fib))

-- reverse e' gia' una funzione di libreria

reverse1 [] = []
reverse1 (x:xs) = reverse xs ++ [x]

-- ma reverse1 complessita' quadratica
-- concatenzione ha costo proporzionale a lunghezza prima lista
-- operatore ++ lineare sul primo argomento

reverse2 xs = reverse2' xs [] where
  reverse2' [] ys = ys
  reverse2' (x:xs) ys = reverse2' xs (x:ys)

-- reverse2 ha anche ricorsione di coda

reverse3 :: [a] -> [a]
reverse3 l = foldl (\xs x -> x:xs) [] l
-- foldl prende per primo l'elemento calcolato in precedenza
-- e qui si tratta di una lista (xs la lista gia' invertita)
-- e poi la testa della lista su cui deve lavorare

reverse4 :: [a] -> [a]
reverse4 l = foldr (\x xs -> xs ++ [x]) [] l
-- pero' qui sono costretto a usare la concatenazione di liste

sumeven [] = 0
sumeven (x:xs) | (mod x 2) == 0 = x + sumeven xs
               | otherwise = sumeven xs

sumodd = \ys ->
  case ys of [] -> 0
             (x:xs) | (mod x 2) == 0 -> sumodd xs
                    | True -> x + sumodd xs

data Point1 = Pt1 {xCoord, yCoord :: Float}
x1 = Pt1 4 5
y1 = xCoord x1
y2 = yCoord x1

-- sommaPari con ricorsione di coda
sommaPari2 xs = sommaPariAux xs 0
  where sommaPariAux [] n = n
        sommaPariAux (x:xs) n | (mod x 2) == 0 = sommaPariAux xs (x+n)
                              | otherwise = sommaPariAux xs n

-- sommaPari con foldl
sommaPari3 xs = foldl (\acc x -> if (mod x 2) == 0 then x + acc else acc) 0 xs

-- sommaPari con foldr
sommaPari4 xs = foldr (\x acc -> if (mod x 2) == 0 then x + acc else acc) 0 xs

-- map con pattern matching
map1 f [] = []
map1 f (x:xs) = f x : map1 f xs

-- map con case
map2 f xs = case xs of
  [] -> []
  (x:xs) -> f x : map2 f xs

-- map con ricorsione di coda
map3 f xs = map3Aux f xs []
  where map3Aux f [] ys = reverse ys
        map3Aux f (x:xs) ys = map3Aux f xs (f x:ys)
-- potevo fare l'append ma costava di piu'

-- map con foldl
map4 f xs = foldl (\xs x -> xs ++ [f x]) [] xs

-- map con foldr
map5 f xs = foldr (\x xs -> f x : xs) [] xs

data BTree a = Null | BTree a (BTree a) (BTree a)

sumBT Null = 0
sumBT (BTree n tL tR) = n + sumBT tL + sumBT tL

depthBT Null = 0
depthBT (BTree n tL tR) = 1 + max (depthBT tL) (depthBT tR)

takeDepthBT 0 _ = Null
takeDepthBT _ Null = Null
takeDepthBT n (BTree a tL tR) = (BTree a (takeDepthBT (n-1) tL) (takeDepthBT (n-1) tR))

insertBST x Null = BTree x Null Null
insertBST x (BTree y tL tR) = if x < y then (BTree y (insertBST x tL) tR) else (BTree y tL (insertBST x tR))
-- eventualmente si poteva scrivere con le guardie invece dell if then else

-- da albero a lista con visita anticipata
visitBST Null = []
visitBST (BTree y tL tR) = visitBST tL ++ y : visitBST tR
-- l'operazione di concatenazione non la rende molto efficiente
-- per concatenare deve svolgere tutta la prima lista

visitBST2 tree = visitBST2Aux tree [] where
  visitBST2Aux Null acc = acc
  visitBST2Aux (BTree x tL tR) acc = visitBST2Aux tL (x : visitBST2Aux tR acc) 

b1 = (insertBST 12 (insertBST 67 (insertBST 23 (insertBST 56 (insertBST 31 (insertBST 23 (insertBST 40 Null)))))))

-- da lista a albero
listToBST xs = listToBSTAux xs Null where
  listToBSTAux [] bst = bst
  listToBSTAux (x:xs) bst = listToBSTAux xs (insertBST x bst)

-- quindi da lista posto costrure albero e poi fare albero
-- e ottenere un algoritmo di ordinamento

-- ma si potrebbe anche definire con foldl
listToBST2 xs = foldl (\bst x -> insertBST x bst) Null xs

-- oppure con foldr
listToBST3 xs = foldr insertBST Null xs
-- potevo anche scriverle senza xs da argomenti e corpo

-- ottengo alberi diversi con le due ...

ordina xs = visitBST2 (listToBST3 xs)
