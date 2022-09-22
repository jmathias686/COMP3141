toCartesian :: (Double, Double) -> (Double, Double)

{-
toCartesian (r, theta) = (x,y)
    where
        y = r * sin theta
        x = r * cos theta
-}

toCartesian = \(r, theta) -> let
                      y = r * sin theta
                      x = r * cos theta
                 in (x,y)




twice :: (a -> a) -> (a -> a)
twice f a = f (f a) -- Equation 1

thrice f a = f (f (f a))


double :: Int -> Int
double x = x * 2




{-
    twice twice double 3
==  (twice twice double) 3 -Equation 1
==  (twice (twice double)) 3
==  (twice quadruple) 3 - defn. of quadruple
==  quadruple (quadruple 3) - equation1
==  48
-}

quadruple :: Int -> Int
quadruple = twice double --defn of quadruple


myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = (f x) : (myMap f xs)


sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum xs

concat' :: [[a]]-> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat xss


foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = x `f` (foldr' f z xs)


sum'' = foldr' (+) 0
concat'' = foldr' (++) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) = if p x then x : filter' p xs
                         else filter' p xs
{-
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

-}




