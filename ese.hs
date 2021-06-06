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

-- 3.1 matrix_dim data una matrice ne calcola le dimensioni, se la matrice è
-- ben formata, altrimenti restituisce (-1,-1)
matrix_dim :: [[a]] -> (Int,Int)
matrix_dim [] = (0,0)
matrix_dim [x] = (1,length x)
matrix_dim (x1:x2:xs) = if length x1 /= length x2
                        then (-1,-1)
                        else (row, col) where
  row = 2 + fst (matrix_dim xs)
  col = if matrix_dim (x2:xs) == (-1,-1)
            then (-1,-1)
                 else snd (matrix_dim (x2:xs))

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
