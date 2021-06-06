factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n-1)


--
repeatString n str = repeatHelper n str ""

repeatHelper 0 _   result = result
repeatHelper n str result = repeatHelper (n-1) str (result++str)


-- fibonacci numbers, fast version
fibonacci :: Integer -> Integer
fibonacci n = fibonacci' 0 1 n

fibonacci' :: Integer -> Integer -> Integer -> Integer
fibonacci' a b 1 = b
fibonacci' a b n = fibonacci' b (a+b) (n-1)


--
describe :: Int -> String
describe n
  | n==2      = "Two"
  | even n    = "Even"
  | n==3      = "Three"
  | n>100     = "Big!!"
  | otherwise = "The number "++show n


--
factorial2 n
  | n<0       = -1
  | n==0      = 1
  | otherwise = n * factorial2 (n-1)


--
guessAge :: String -> Int -> String
guessAge "Griselda" age
    | age < 47 = "Too low!"
    | age > 47 = "Too high!"
    | otherwise = "Correct!"
guessAge "Hansel" age
    | age < 12 = "Too low!"
    | age > 12 = "Too high!"
    | otherwise = "Correct!"
guessAge name age = "Wrong name!"


-- given a password, return (Just username) if login succeeds, Nothing otherwise
login :: String -> Maybe String
login "f4bulous!" = Just "unicorn73"
login "swordfish" = Just "megahacker"
login _           = Nothing

data IntList = Empty | Node Int IntList
  deriving Show

ihead :: IntList -> Int
ihead (Node i _) = i

itail :: IntList -> IntList
itail (Node _ t) = t

ilength :: IntList -> Int
ilength Empty = 0
ilength (Node _ t) = 1 + ilength t
