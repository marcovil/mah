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

