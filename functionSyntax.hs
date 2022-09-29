lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "sry"                     -- Catch-all at function end

-- Recursively-defined function
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- "Non-exhaustive patterns in function charName"
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

-- Vector addition utilizing pattern matching
vectorAdd :: (Num a) => (a, a) -> (a, a) -> (a, a)
vectorAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Head function reimplementation...
head' :: [a] -> a
head' [] = error "NOPE"
head' (x:_) = x

-- ...and Last (extra)
last' :: [a] -> a                   -- Alternatively use monads(?) [a] -> Maybe a...
last' [] = error "NOPE"             -- ... and [] = Nothing
last' [x] = x                       -- Equivalent to (x:[])
last' (_:xs) = last' xs

-- Length function reimplementation
length' :: (Num b) => [a] -> b
length' [] = 0                      -- 通称Edge condition（限りとしてとか）
length' (_:x) = 1 + length' x       -- Notice the word consumption by _: until []

-- Sum function
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Patterns
capital :: String -> String
capital "" = "EMPTY"
capital all@(x:xs) = "First letter of " ++ all ++ " is: " ++ [x]

