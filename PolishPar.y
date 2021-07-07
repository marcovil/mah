{
module Main where  
import PolishTok
}

%name calc
%tokentype { Token }
%error { parseError }

%token 
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenMult }
      '/'             { TokenDiv }
      int             { TokenInt $$ } 

%%

Exp : '+' Exp Exp     { $2 + $3 }
    | '-' Exp Exp     { $2 - $3 }
    | '*' Exp Exp     { $2 * $3 }
    | '/' Exp Exp     { $2 `div` $3 }
    | int             { $1 }  

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

main = do
  s <- getContents
  print (calc (lexer s))
}
