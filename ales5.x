-- Selezionare in un file di testo le stringhe di caratteri che
-- rappresentano un numero multiplo di quattro,
-- nell’usuale notazione decimale. Solo i numeri multipli di 4
-- vengono stampati in uscita, separati da uno spazio,
-- la restante parte del testo viene eliminata. Il controllo di
-- divisibilità deve essere implementato usando opportune
-- espressioni regolari, e non l’operazione di divisione-resto.

{
module Main (main) where
}

%wrapper "basic"

$digit = [0-9]
$even  = [0 2 4 6 8]
$odd   = [1 3 5 7 9]

tokens :-

  0|4|8					{ \s -> s ++ " " }
  $digit* $even (0|4|8)  		{ \s -> s ++ " " }
  $digit* $odd (2|6)			{ \s -> s ++ " " }
  $digit+      				;
  .|\n					;

{
main = do
  s <- getContents
  putStr (concat (alexScanTokens s))
}
