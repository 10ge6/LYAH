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

-- Some reimplementations...
head' :: [a] -> a
head' [] = error "NOPE"
head' (x:_) = x

last' :: [a] -> a                   -- Alternatively use monads(?) [a] -> Maybe a...
last' [] = error "NOPE"             -- ... and [] = Nothing
last' [x] = x                       -- Equivalent to (x:[])
last' (_:xs) = last' xs

length' :: (Num b) => [a] -> b
length' [] = 0                      -- 通称Edge condition（限りとしてとか）
length' (_:x) = 1 + length' x       -- Notice the word consumption by _: until []

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Patterns
capital :: String -> String
capital "" = "EMPTY"
capital all@(x:xs) = "First letter of " ++ all ++ " is: " ++ [x]

-- Guards
densityTell :: (Fractional a, Ord a) => a -> a -> String     -- Alternatively declare typeclass of a to be RealFloat (subclass of Fractional, implies Ord)
densityTell mass volume
    | density < air    = "fly"
    | density <= water = "float"
    | otherwise              = "sink"
    where density = mass / volume                            -- Names defined here are only visible to this function
          (air, water) = (1.2, 1000.0)

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b                 -- Writing these inline is unreadable (max' a b | a > b = a | otherwise = b) but possible

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcDensities :: (RealFloat a) => [(a, a)] -> [a]            -- Conversely, (Fractional a, Ord a) also works
calcDensities xs = [density m v | (m, v) <- xs]
    where density mass volume = mass / volume

{-
 - where bindings can also be nested. It's a common idiom to make a function
 - and define some helper function in its where clause and then to give
 - those functions helper functions as well, each with its own where clause.
-}

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]         -- neat syntax

{-
 - since let bindings are expressions and are fairly local in their scope,
 - they can't be used across guards. Some people prefer where bindings because the
 - names come after the function they're being used in. That way, the function
 - body is closer to its name and type declaration and to some that's more readable.
-}

head' :: [a] -> a
head' xs = case xc of [] -> error "No head for empty lists!"    -- pattern matching on parameters in function definitions is
                      (x:_) -> x                                -- syntactic sugar for case expressions (they're interchangeable)

{-
 - whereas pattern matching on function parameters can only be done when defining
 - functions, case expressions can be used pretty much anywhere. for instance:
-}

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
					       xs -> "a longer list."

{-
 - because pattern matching in function definitions is synctactic sugar
 - for case expressions, we could have also defined this like so:
-}

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
	  what xs = "a longer list."

