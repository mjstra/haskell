
--one way
head' :: [a] -> a
head' [] = error "No empty lists"
head' (x:_) = x

--or another
head' :: [a] -> a
head' xs = case xs of [] -> error "nope" (x:_) -> x


describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs      
                   where what [] = "empty." 
                   
                            --[x] ? works
                             what [x] = "a singleton list."            
                             what xs = "a longer list."


maxi :: (Ord a) => [a] -> a
maxi [] = error "max of empty"
maxi [x] = x
maxi (x:xs)
    | x > curr = x
    | otherwise = curr
    where curr = maxi xs --only compute on time
    --adhoc definition

maximum (x:xs) --bad since you compute maximum 2 times!!
    | (maximum xs) > x = maximum xs --bad
    | otherwise = x

revers :: [a] -> [a]
revers [] = error "empty"
revers (x:xs) = revers xs ++ [x]
reversx= foldl (flip (:)) []

reverser = go [] --go == helper function
    where 
        go acc [] = acc
        go acc (x:xs) = go (x:acc) xs

--or with foldl
reverseL = foldl(\acc, x -> x:acc) [] --or foldl' , which is lazy
reve = foldl(flip (:))[]

isInside :: (Eq a) => a -> [a] -> Bool
isInside a [] = False
isInside a (x:xs) = 
    | a == xs = True
    | otherwise = isInside a xs

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip [] _ = []
zip'(x:xs) (y:ys) = (x,y):zip' xs ys --pair of heads with zip of tail
--two lists zipped are equal to pairing up their 
--heads and then tacking on the zipped tails. Zipping [1,2,3]
-- and ['a','b'] will eventually try to zip [3] with []. The edge condition patterns kick in and so the
-- result is (1,'a'):(2,'b'):[], which is exactly the same as [(1,'a'),(2,'b')].


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
        -- func  arg1 arg2   list1  list2  zippedlist
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y zipWith' f xs ys 

flip' :: (a -> b -> c) -> (b -> a -> c)
--func in:a in:b out:c    
flip' f = g
        where g x y = f y z
--or !!
--flip' f x y = f y x


--zipWith (flip' div) [2,2..] [10,8,6,4,2]  
--[5,4,3,2,1] 

map' :: (a -> b) -> [a] -> [b]
map' f (x: xs) = f x : map' f xs

filter' :: (a -> Bool b) -> [a] -> [b]
filter' pred (x:xs) = 
    | pred x        = x : filter' p xs
    | otherwise     = filter' p xs


--    find the largest number under 100,000 that's divisible by 3829
findLargest :: (Number a) => a -> a
findLargest head (filter p [100000, 99999,..])
            where p = a 'mod' 3829 == 0

--find the sum of all odd squares that are smaller than 10,000.

sumOddSquares = sum (takeWhile (<10000)) (filer odd (map (^2) [1..] ))

sumOdd = sum takeWhile [ x^2 | x <- xs, odd (n^2)]


chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain a 
    | odd a = a: chain (a * 3 + 1)
    | otherwise = a:chain(a/2) 


numLongChains :: Int 
numLongChains = length (filter isLong (map chain [1..100]))
        where isLong xs = length xs > 15


listOfMult = map (*) [0..]
listOfMult !! 4 5

elem' :: a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if val == a then True else acc) False ys
--semigroup (All)


map' :: (a -> b) -> [a] -> [b]
map' f xs = foldl(\acc val -> acc ++ [f val])[] xs

maximum' :: (Ord a) => [a] -> a
maximum' = foldrl (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc val -> va; : acc) []

product' :: (Num a) => [a] -> a
product' = foldrl(*)
--foldl (\acc x -> x * acc) 1 [1,2,3]

filter' :: (a -> Bool) -> [a] -> a
filter' p = foldl(\acc val -> if p val then val:acc else acc)[]

sqrtSums:: Int
sqrtSums = length (takeWhile (<1000) (scanl (+) 1 (map sqrt [1..]))


zip' :: List a -> List b -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

sqrt:: Int
sqrtSums = takeWhile (<1000) map sqrt [1..] 

map ($ 3) [(4+), (10*), (^2), sqrt] 
--define composition

search:: (Eq a) => [a] -> [a] -> Bool
search needle haystack = foldl (\acc val -> acc or needle==val ) False