doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

length' xs = sum [1 | _ <- xs]

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

largestDivisible :: Integer
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n  = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x ) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc ->if p x then x : acc else acc) []

phonebook =
  [("betty", "555-2938")
  ,("bonnie", "452-2928")
  ,("pasty", "493-2928")
  ,("lucille", "205-2928")
  ,("wendy", "939-8282")
  ,("penny", "853-2492")]

findkey :: (Eq k) => k -> [(k, v)] -> Maybe v
findkey key xs = foldr
                   (\(k, v) acc -> if key == k then Just v else acc)
                   Nothing xs
