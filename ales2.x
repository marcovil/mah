-- Rimuovere i commenti da un testo Haskell. I commenti Haskell, hanno due possibili formati
-- iniziano con la stringa di caratteri -- e terminano con il ritorno a capo
-- iniziano con la stringa di caratteri {- e terminano con la stringa -}
-- Si cerchi inoltre di risolvere il problema di riconoscere coppie di commenti innestati,
-- ossia la stringa {- aa {- bb -} cc -} viene riconosciuta come un singolo commento
-- e non come il commento {- aa {- bb -} seguito dalla stringa cc -}

{
  module Main (main) where
}

%wrapper "basic"

tokens :-

"--".*\n		{ \s -> '\n' }
\n*"{-"(.|\n)*"-}" 	{ \s -> '\n' }
.|\n			{ \s -> (head s) }

{
main = do
  s <- getContents
  print (alexScanTokens s)
  putStr (simplify (alexScanTokens s)) where
   simplify [] = []
   simplify ('\n':'\n':'\n':xs) = simplify ('\n':'\n':xs)
   simplify (x:xs) = x : simplify xs
}
