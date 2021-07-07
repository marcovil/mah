{
module PolishTok (Token(..),lexer) where
}

%wrapper "basic"

$digit = 0-9

tokens :-

 $white+	;
 \+		{ \s -> TokenPlus }
 \-		{ \s -> TokenMinus }
 \*		{ \s -> TokenMult }
 \/		{ \s -> TokenDiv }
 \-? $digit+	{ \s -> TokenInt (read s) }

{
data Token =
 TokenPlus |
 TokenMinus |
 TokenMult |
 TokenDiv |
 TokenInt Int
  deriving (Eq,Show)

lexer = alexScanTokens
}
