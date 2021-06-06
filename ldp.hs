primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs , x `mod` p /= 0]

data Point a = Pt a a

reverse1 :: [a] -> [a]
reverse1 l = foldl (\xs x -> x:xs) [] l

reverse2 :: [a] -> [a]
reverse2 l = foldr (\x xs -> xs ++ [x]) [] l
