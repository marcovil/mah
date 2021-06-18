-- 1.1
fact :: Int -> Int
fact n = factacc n 1
         where factacc 1 m = m
               factacc n m = factacc (n-1) (n*m)

-- 1.2
binom :: Int -> Int -> Int
binom n k | k < 0 || k > n = 0
          | k == 0 || n <= 1 = 1
          | otherwise = binom (n-1) k + binom (n-1) (k-1)

-- 2.1 data una lista ne costruisce una rimuovendo gli elementi
-- di posizione pari (contando da 1)
rep :: [a] -> [a]
rep [] = []
rep [_] = []
rep (x1:x2:xs) = x2 : rep xs

-- 2.2 somma gli elementi di posizione dispari di una lista
-- (contando da 1)
so :: (Num a) => [a] -> a
so [] = 0
so [x] = x
so (x1:x2:xs) = x1 + so xs

-- 2.5 funzione che costruisce, a partire da una lista di numeri interi,
-- una lista di coppie in cui
-- (a) il primo elemento di ogni coppia è uguale all’elemento di
-- corrispondente posizione nella listaoriginale e
-- (b) il secondo elemento di ogni coppia è uguale alla somma di
-- tutti gli elementi conseguenti della lista originale.
listc :: [Int] -> [(Int,Int)]
listc [] = []
listc (x:xs) = (x,foldr (+) 0 xs) : (listc xs)

-- 3.1 matrix_dim data una matrice ne calcola le dimensioni, se la matrice è
-- ben formata, altrimenti restituisce (-1,-1)
matrix_dim :: [[a]] -> (Int,Int)
matrix_dim [] = (0,0)
matrix_dim mat = let cols = map length mat in
  if same cols
     then (length mat, head cols)
          else (-1,-1)

same :: (Eq a) => [a] -> Bool
same [] = True
same [_] = True
same (x1:x2:xs) = if x1 == x2
                     then True && same (x2:xs)
                          else False

-- 3.2 colsums data una matrice calcola il vettore delle somme delle colonne
colsums :: Num a => [[a]] -> [a]
colsums ([]:xs) = []
colsums xss = sumfirsts : sumrest where
  sumfirsts = sum (map (\y -> (head y)) xss)
  sumrest = colsums (map (\y -> (tail y)) xss)

data BST a = Void | Node {
  val :: a ,
  left , right :: BST a
  } deriving ( Eq , Ord , Read , Show )

-- 4.1 funzione che calcola la somma dei valori di un albero a valori sommabili
treesum :: Num a => BST a -> a
treesum Void = 0
treesum (Node x l r) = x + treesum l + treesum r

-- 4.2 funzione che calcola la somma dei valori dispari di un albero
-- a valori sommabili su cui sia utilizzabile la funzione odd.
sumoddtree :: Integral a => BST a -> a
sumoddtree Void = 0
sumoddtree (Node x l r) = if odd x
                             then x + sumoddtree l + sumoddtree r
                                  else sumoddtree l + sumoddtree r

-- 4.3 funzione che da una lista di alberi determina se le somme
-- dei valori degli alberi sono uguali tra loro
samesums :: (Num a, Eq a) => [BST a] -> Bool
samesums l = same (map treesum l)

data Tree a = Voidgen | Nodegen a [ Tree a ]
  deriving ( Eq , Show )

-- 5.1 generalizzazione della foldr per alberi generici

--

data QT a = C a | Q ( QT a ) ( QT a ) ( QT a ) ( QT a )
  deriving ( Eq , Show )

-- 6.1 da 4 QuadTree a un QuadTree
buildNSimplify :: ( Eq a, Show a ) => QT a -> QT a -> QT a -> QT a -> QT a
buildNSimplify (C x) (C y) (C z) (C w) = if x == y && y == z && z == w
                                         then C x
                                         else Q (C x) (C y) (C z) (C w)
buildNSimplify q1 q2 q3 q4 = Q q1 q2 q3 q4

c1 = C 1
c2 = C 2
c3 = C 3
c4 = C 4
c19 = C 19
c20 = C 20
qt1 = Q c1 c2 c3 c3
qt2 = Q c2 c2 c3 c1
qt3 = Q c3 c3 c3 c2
qt4 = Q c3 c3 c2 c1
qt5 = Q c1 c1 c1 c1
qt6 = Q qt1 qt2 qt3 qt1

-- 6.2 da un termine di tipo QT a un QuadTree
simplify :: ( Eq a, Show a ) => QT a -> QT a
simplify (Q (C x) (C y) (C z) (C w)) = if x == y && y == z && z == w
                                       then C x
                                       else Q (C x) (C y) (C z) (C w)
simplify (Q x1 x2 x3 x4) = (Q x1 x2 x3 x4) 

-- 6.3 data funzione f e QuadTree q mapQT applica f a tutti i pixel
-- dell'immagine codificata da q
mapQT :: ( Eq a, Show a ) => (a -> b) -> QT a -> QT b
mapQT f (C x) = C (f x)
mapQT f (Q x1 x2 x3 x4) = Q (mapQT f x1) (mapQT f x2) (mapQT f x3) (mapQT f x4)

-- 6.4 dato un QuadTree determina il numero (minimo) di
-- pixel di quell’immagine
howManyPixels :: ( Eq a, Show a ) => QT a -> Int
howManyPixels (C _) = 1
howManyPixels (Q x1 x2 x3 x4) = let hmp1 = howManyPixels x1
                                    hmp2 = howManyPixels x2
                                    hmp3 = howManyPixels x3
                                    hmp4 = howManyPixels x4
                                    in 4 * maximum [hmp1, hmp2, hmp3, hmp4]

z = C 0
u = C 1
q = Q z u u u

-- 6.5 dato colore c e lista di QuadTrees costruisce lista QuadTrees che
-- codificano immagini i cui colori sono limitati a c (pixel originale
-- se colore e' < c, c altrimenti
limitAll :: ( Eq a, Show a, Ord a ) => a -> [QT a] -> [QT a]
limitAll c [] = []
limitAll c (x:xs) = mapQT (\x -> if x < c then x else c) x : limitAll c xs

qt7 = Q c19 c20 c4 c3
qt8 = Q c4 c3 c3 c2
qt9 = Q c20 c20 c20 c20
qt10 = c20
qt11 = Q qt7 qt8 qt1 qt9
