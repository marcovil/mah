-- 1.1 fattoriale
fact :: Integer -> Integer
fact n = factacc n 1
         where factacc 1 m = m
               factacc n m = factacc (n-1) (n*m)

-- 1.2 coefficiente binomiale
binom :: Integer -> Integer -> Integer
binom n k | k < 0 || k > n = 0
          | k == 0 || n <= 1 = 1
          | otherwise = binom (n-1) k + binom (n-1) (k-1)

{- 2.1 data una lista ne costruisce una rimuovendo gli elementi
di posizione pari (si conta partendo da 1) -}
ree :: [a] -> [a]
ree [] = []
ree [x] = [x]
ree (x1:x2:xs) = x1 : ree xs

{- 2.2 somma gli elementi di posizione dispari di una lista
(si conta partendo da 1) -}
so :: (Num a) => [a] -> a
so [] = 0
so [x] = x
so (x1:x2:xs) = x1 + so xs

-- 2.3 quicksort polimorfo
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort bigger where
  smaller = [y | y <- xs, y <= x]
  bigger = [y | y <- xs, y > x]

{- 2.5 funzione che costruisce, a partire da una lista di numeri interi,
una lista di coppie in cui
(a) il primo elemento di ogni coppia è uguale all’elemento di
corrispondente posizione nella lista originale e
(b) il secondo elemento di ogni coppia è uguale alla somma di
tutti gli elementi conseguenti della lista originale -}
listc :: [Int] -> [(Int,Int)]
listc [] = []
listc (x:xs) = (x,foldr (+) 0 xs) : (listc xs)

{- 2.6 funzione che costruisce, a partire da una lista di numeri interi
una lista di coppie in cui
(a) il primo elemento di ogni coppia è uguale all’elemento
di corrispondente posizione nella lista originale e
(b) il secondo elemento di ogni coppia è uguale alla somma
di tutti gli elementi antecedenti della lista originale -}
couples :: [Int] -> [(Int,Int)]
couples xs = couplesAux xs 0 where
  couplesAux [] acc = []
  couplesAux (x:xs) acc = (x,acc) : couplesAux xs (acc + x)

{- 3.1 matrix_dim data una matrice ne calcola le dimensioni, se la matrice è
ben formata, altrimenti restituisce (-1,-1) -}
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

bst1 = Void
bst2 = Node 72 (Node 33 Void Void) (Node 91 (Node 84 Void Void) (Node 105 Void Void))
bst3 = Node 19  (Node 13 Void Void) (Node 46 (Node 27 Void Void) (Node 59 Void Void))
bst4 = Node 6 Void Void

-- 4.1 calcola la somma dei valori di un albero a valori sommabili
treesum :: Num a => BST a -> a
treesum Void = 0
treesum (Node x l r) = x + treesum l + treesum r

{- 4.2 calcola la somma dei valori dispari di un albero
a valori sommabili su cui sia utilizzabile la funzione odd -}
sumoddtree :: Integral a => BST a -> a
sumoddtree Void = 0
sumoddtree (Node x l r) = if odd x
                             then x + sumoddtree l + sumoddtree r
                                  else sumoddtree l + sumoddtree r

{- 4.3 da una lista di alberi determina se le somme
dei valori degli alberi sono uguali tra loro -}
samesums :: (Num a, Eq a) => [BST a] -> Bool
samesums l = same (map treesum l)

-- 4.4 determina se un valore e' presente in un BST
bstElem :: ( Ord a ) => a -> BST a -> Bool
bstElem y Void = False
bstElem y (Node x l r) = if y == x then True else bstElem y l || bstElem y r

-- 4.5 inserimento di valore x in albero t
insertInBST :: ( Ord a ) => a -> BST a -> BST a
insertInBST n Void = Node n Void Void
insertInBST n (Node x l r) = if n <= x then Node x (insertInBST n l) r else Node x l (insertInBST n r)

-- 4.6 lista ordinata degli elementi di un BST
bst2List :: BST a -> [a]
bst2List tree = bst2List' tree [] where
  bst2List' Void acc = acc
  bst2List' (Node x l r) acc = bst2List' l (x : bst2List' r acc)

-- 4.7 ordinamento di liste usando funzione precedente
sortList :: ( Ord a ) => [a] -> [a]
sortList xs = sortListAux xs Void where
  sortListAux [] bst = bst2List bst
  sortListAux (x:xs) bst = sortListAux xs (insertInBST x bst)

{- 4.8 lista ordinata di tutti gli elementi dell'albero t che soddisfano
il predicato p -}
filterTree :: ( Ord a ) => (a -> Bool) -> BST a -> [a]
filterTree p t = filterTreeAux p t [] where
  filterTreeAux p Void acc = acc
  filterTreeAux p (Node x l r) acc = if p x
                                        then filterTreeAux p l (x : filterTreeAux p r acc)
                                             else filterTreeAux p l (filterTreeAux p r acc)


data Tree a = VoidT | NodeT a [ Tree a ]
  deriving ( Eq , Show )

alg1 = NodeT 50 [NodeT 60 [], NodeT 70 [VoidT], NodeT 80 [VoidT,VoidT], NodeT 90 [NodeT 180 [NodeT 60 [VoidT,VoidT]]]]
alg2 = NodeT 50 []
alg3 = NodeT 50 [NodeT 50 [], NodeT 60 [], NodeT 70 []]

-- 5.1 generalizzazione della foldr per alberi generici
treefold :: ( Eq a , Show a ) => (a -> [b] -> b ) -> b -> Tree a -> b
treefold f z VoidT = z
treefold f z (NodeT x [])  = f x [z]
treefold f z (NodeT x xs) = f x (foldr (\x base -> treefold f z x : base) [] xs)

-- 5.2 altezza albero generico usando la treefold
height :: (Eq a, Show a) => Tree a -> Int
height tree = treefold (\x xs -> 1 + (maximum xs)) (-1) tree

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
c5 = C 1
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

{- 6.3 data funzione f e QuadTree q mapQT applica f a tutti i pixel
dell'immagine codificata da q -}
mapQT :: ( Eq a, Show a ) => (a -> b) -> QT a -> QT b
mapQT f (C x) = C (f x)
mapQT f (Q x1 x2 x3 x4) = Q (mapQT f x1) (mapQT f x2) (mapQT f x3) (mapQT f x4)

{- 6.4 dato un QuadTree determina il numero (minimo) di
pixel di quell’immagine -}
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
t = (Q q (C 0) (C 2) q)

{-6.5 dato colore c e lista di QuadTrees costruisce lista QuadTrees che
codificano immagini i cui colori sono limitati a c (pixel originale
se colore e' < c, c altrimenti -}
limitAll :: ( Eq a, Show a, Ord a ) => a -> [QT a] -> [QT a]
limitAll c [] = []
limitAll c (x:xs) = mapQT (\x -> if x < c then x else c) x : limitAll c xs

qt7 = Q c19 c20 c4 c3
qt8 = Q c4 c3 c3 c2
qt9 = Q c20 c20 c20 c20
qt10 = c20
qt11 = Q qt7 qt8 qt1 qt9

data Mat a = Mat {
  nexp :: Int,
  mat :: QT a
  }
  deriving (Eq, Show)

-- 7.1 determina se matrice triangolare inferiore
lowertriangular :: (Eq a, Num a, Show a) => Mat a -> Bool
lowertriangular (Mat 0 (C _)) = True
lowertriangular (Mat n (C 0)) = True
lowertriangular (Mat n (C _)) = False
lowertriangular (Mat n (Q x (C 0) w z)) = lowertriangular (Mat (n-1) x) &&
                                          lowertriangular (Mat (n-1) z)
lowertriangular (Mat n (Q x y w z)) = False
  
m0 = Mat 0 (C 2)
m1 = Mat 0 (C 0)
m2 = Mat 1 (C 0)
m3 = Mat 1 (C 4)
m4 = Mat 1 (Q (C 2) (C 0) (C 1) (C 3))
m5 = Mat 1 (Q (C 2) (C 4) (C 5) (C 6))
m6 = Mat 1 (Q (C 0) (C 3) (C 3) (C 0))
m7 = Mat 2 (C 0)
m8 = Mat 2 (C 2)
m9 = Mat 2 (Q qt7 qt8 qt7 qt9)
m10 = Mat 2 (Q qt7 (C 0) qt8 qt9)
m11 = Mat 2 (Q (Q (C 2) (C 0) (C 1) (C 3)) (C 0) qt7 (Q (C 2) (C 0) (C 1) (C 3)))
     

