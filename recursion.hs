maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"     -- edge
maximum' [x] = x                                -- conditions
maximum' (x:xs)                                 -- split list into a head and a tail - very common idiom when doing recursion with lists
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs                 -- pattern matching goes well with recursion

-- for an elegant rewrite:
maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)         -- in essence, the maximum of a list is the max of the first element and the maximum of the tail

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []                            -- guards here instead of patterns because we're testing for a boolean condition
    | otherwise = x:replicate' (n-1) x

{-
 - note Num is not a subclass of Ord. this is because not every number type has an ordering, e.g. complex numbers aren't ordered.
 - so that's why we have to specify both the Num and Ord class constraints when doing addition or subtraction and also comparison.
-}

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs

-- CLEAN
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

{-
 - take head
 - put it in the middle of two lists that are respectively smaller/equal to and bigger than it
 - eventually, so much gets broken up we reach empty lists and thus the edge condition
 - [5,1,9,4,6,7,3]
 - [1,4,3] ++ [5] ++ [9,6,7]
 - [] ++ [1] ++ [4,3]  [6,7] ++ [9] ++ []
 - [3] ++ [4] ++ []  [] ++ [6] ++ [7]
 - [] ++ [3] ++ []  [] ++ [7] ++ []
-}

{-
 - to summarize, the pattern goes:
 - defining an edge case, then defining a function that
 - does something between some element and the function applied to the rest
 -
 - a sum is the first element of a list plus the sum of the rest of the list
 - a product of a list is the first element of the list times the product of the rest of the list
 - etc
 -
 - usually the edge case is some scenario where a recursive application doesn't make sense
 - e.g. when dealing with lists, the empty list. trees, node with no children.
 - remember factorial definition?
 -
 - often edge cases turn out to be identities. (multiplication by 1 = 1, 0 is the identity of addition)
 - in quicksort, the edge case is the empty list and so is the identity
 - (adding an empty list to a list returns the original list)
 -
 - when trying to think of a recursive way to solve a problem, try to think of when
 - a recursive solution doesn't apply and see if you can use that as an edge case, think
 - about identities and think about whether you'll break apart the parameters of the function
 - (how lists are usually broken into head:tail via pattern matching)
 - and which part the recursive call is going to affect
-}

