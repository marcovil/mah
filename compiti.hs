checkpermutation :: Eq a => [a] -> [a] -> Bool
checkpermutation [] [] = True
checkpermutation [] (y:ys) = False
checkpermutation (x:xs) ys = if ismember x ys
                                then checkpermutation xs (remove x ys)
                                     else False

ismember :: Eq a => a -> [a] -> Bool
ismember x [] = False
ismember x (y:ys) = if x == y then True else ismember x ys

remove :: Eq a => a -> [a] -> [a]
remove x (y:ys) = if x == y then ys else y : remove x ys

coppie3 xs ys = foldl (\base x -> (foldl (\listagen y -> (x,y):listagen) [] ys) ++ base) [] xs

coppie4 xs ys = map (\x -> (map (\y -> (x,y)) ys)) xs

unfoldl [] = []
unfoldl (x:xs) = x ++ unfoldl xs

unfoldl2 xss = foldl (\listagen x -> x ++ listagen) [] xss

apply graph [] = []
apply graph (x:xs) = if fst (checkx x graph)
                        then (snd (checkx x graph)) : apply graph xs
                             else apply graph xs

checkx x [] = (False,0)
checkx x (y:ys) = if x == fst y
                     then (True, snd y)
                          else checkx x ys

gapp [] x = Nothing
gapp ((x1,y1):f) x | x == x1 = Just y1
                   | otherwise = gapp f x

gmap f xs = myfilter (map (gapp f) xs)

myfilter [] = []
myfilter (Nothing:xs) = myfilter xs
myfilter ((Just x):xs) = x : myfilter xs

factors 1 = []
factors 2 = [2]
factors n = let (a,b) = checkfactors n (n-1) in
  b : factors a

checkfactors :: Integral a => a -> a -> (a,a)
checkfactors n 1 = (1,n)
checkfactors n x = if (n `mod` x) == 0
                      then (x, n `div` x)
                           else checkfactors n (x-1)

primes :: (Eq a, Ord a) => [a] -> [a] -> [a]
primes xs [] = xs
primes [] xs = xs
primes (x:xs) (y:ys) |  x < y = x : primes xs (y:ys)
                     |  x == y = x : primes xs ys
                     |  x > y = y : primes (x:xs) ys

mcm :: (Eq a, Ord a, Integral a) => a -> a-> a
mcm x y = mult (primes (factors x) (factors y)) where
  mult [] = 1
  mult (x:xs) = x * mult xs

mcm2 :: (Eq a, Ord a, Integral a) => a -> a-> a
mcm2 x y = foldr (\x already -> x * already) 1 (primes (factors x) (factors y))

scacchiera :: Int -> [[Int]]
scacchiera 0 = []
scacchiera 1 = [[0]]
scacchiera n = (riga n) : (modifica (scacchiera (n-1)))

riga :: Int -> [Int]
riga 0 = []
riga 1 = [0]
riga n = 0:1:riga (n-2)

modifica :: [[Int]] -> [[Int]]
modifica [] = []
modifica (x:xs) | (head x) == 0 = (1:x) : modifica xs
                | (head x) == 1 = (0:x) : modifica xs

fun :: Ord a => [a] -> [a] -> [a]
fun xs [] = xs
fun [] ys = ys
fun (x:xs) (y:ys) | x < y = x : fun xs (y:ys)
                  | x == y = x : fun xs ys
                  | x > y = y : fun (x:xs) ys

mat2list :: Ord a => [[a]] -> [a]
mat2list [] = []
mat2list (x:[]) = x
mat2list (x:xs) = mat2list ((fun x (head xs)):(tail xs))
