double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

n = a `div` length xs
    where
    a = 10
    xs = [1,2,3,4,5]

mylast ns = ns !! (length ns - 1)

mylast2 [a] = a
mylast2 (a:as) = mylast2 as

mylast3 xs = head (reverse xs)

myinit ns = take (length ns - 1) ns

myinit2 [a] = []
myinit2 (a:as) = a : myinit2 as

myinit3 xs = reverse (tail (reverse xs))

myadd :: (Int, Int) -> Int
myadd (x,y) = x+y

-- lecture4

safetail1 xs = if xs == [] then [] else tail xs

safetail2 xs | xs == [] = []
             | otherwise = tail xs

safetail3 [] = []
safetail3 (x:xs) = xs

-- lecture5

factors1 :: Int -> [Int]
factors1 n = [x | x <- [1..n], n `mod` x == 0]

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n] , y <- [1..n], z <- [1..n], (x^2 + y^2 == z^2)]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init (factors1 x)) == x]

scalar :: [Int] -> [Int] -> Int
scalar xs ys = sum [fst(x,y)*snd(x,y) | (x,y) <- zip xs ys]
-- potevo semplicemente prendere x e y senza fare fst e snd

scalar2 :: [Int] -> [Int] -> Int
scalar2 xs ys = sum [xs !! i * ys !! i | i <- [0..n-1]]
                where n = length xs

-- lecture6

qs :: Ord a => [a] -> [a]
qs [] = []
qs (x:xs) = qs [i | i <- xs, i <= x] ++ [x] ++ qs [ j | j <- xs, j > x]

myand :: [Bool] -> Bool
myand [] = True
myand (x:xs) = if (x == False) then False else myand xs
-- potevo usare x && myand xs

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs
-- era meglio (xs:xss) invece di (x:xs)

myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n x = x : myreplicate (n-1) x

nth :: [a] -> Int -> a
nth (x:xs) 0 = x
nth (x:xs) n = nth xs (n-1)
-- crasha correttamente se gli dai una empty list
-- nella prima equazione possiamo mettere _ al posto di xs e _ al posto di x nella seconda

myelem :: Eq a => a -> [a] -> Bool
myelem i [] = False
myelem i (x:xs) = if (i == x) then True else myelem i xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge ys [] = ys
merge (x:xs) (y:ys) = if (x <= y) then x : merge xs (y:ys) else y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort xs = if length xs <= 1 then xs else merge (msort firsthalf) (msort secondhalf)
  where firsthalf = take ((length xs) `div` 2) xs
        secondhalf = drop ((length xs) `div` 2) xs
-- msort [] = [] e msort [x] = [x] come casi base e poi msort xs caso con chiamate
-- e poi ha fatto una helper function halve con take e drop, simile sostanzialmente

-- lecture 7

insert :: Int -> [Int] -> [Int]
insert i [] = [i]
insert i (x:xs) = if i <= x then i : (x:xs) else x : insert i xs

is :: [Int] -> [Int]
is [] = []
is (x:xs) = insert x (is xs)

-- lecture 8

-- un esempio di foldr
-- foldr (\x y -> 2 * x * y) 1 [1,1,1,1]

flength :: [a] -> Int
flength = foldr (\ _ n -> n + 1) 0

myreverse :: [a] -> [a]
myreverse = foldr (\i x -> x ++ [i]) []

-- 1 - curried functions

-- (map f (filter p xs))

mymap :: (a -> b) -> [a] -> [b]
mymap f = foldr (\ x xs -> (f x) : xs) []

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter p = foldr (\ x xs -> if (p x) then x : xs else xs) [] 

-- lecture 9

-- lecture 10

data Shape = Circle Float
           | Rect Float Float

add :: Int -> Int -> Int
add x y = x + y
-- lui intendeva coi Nat

mult :: Int -> Int -> Int
mult 0 y = 0
mult x y = y + mult (x-1) y
-- lui intendeva coi Nat

data Tree a = Leaf a | Node (Tree a) (Tree a)

generaCoppia nEsercizi matricola = (primo, secondo) where 
  primo = matricola `mod` nEsercizi + 1
  secondo = (matricola `mod` (nEsercizi - 3) + primo + 1) `mod`  nEsercizi + 1
