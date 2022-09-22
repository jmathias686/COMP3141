flip' :: (a ->b ->c) -> b -> a -> c
flip' f left right = f right left


append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys


-- [a, b, c] -> a `f` (b `f` (c `f` base))
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f base [] = base
foldr' f base (x:xs) = x `f` (foldr f base xs)


-- [a, b, c] -> ((acc `f` a) `f` b) `f` c
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f acc [] = acc
foldl' f acc (x:xs) = foldl' f (acc `f` x) xs


map' :: (a -> b) -> [a] -> [b]
map' f = foldr' (\next mapped -> f next:mapped) []


append'' :: [a] -> [a] -> [a]
append'' front back = foldr' (:) back front


filter' :: (a-> Bool) -> [a] -> [a]
filter' p = foldr' step []
    where
        step next filtered =
            if p next
                then next : filtered
                else filtered
            {- | p next = next:filtered
               | otherwise = filtered
            -}

