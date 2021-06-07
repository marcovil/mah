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

reverse1 :: [a] -> [a]
reverse1 l = foldl (\xs x -> x:xs) [] l

reverse2 :: [a] -> [a]
reverse2 l = foldr (\x xs -> xs ++ [x]) [] l
