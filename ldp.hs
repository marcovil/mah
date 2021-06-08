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
