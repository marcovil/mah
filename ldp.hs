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

data BTree a = Null | Bt a (Bt a) (Bt a)

sumBT Null = 0
sumBT (Bt n tL tR) = n + sumBT tL + sumBT tL

depthBT Null = 0
depthBT (Bt n tL tR) = 1 + max (depthBT tL) (depthBT tR)
