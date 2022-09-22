{-# LANGUAGE GADTs, DataKinds, KindSignatures, TupleSections, PolyKinds, TypeOperators, TypeFamilies, PartialTypeSignatures #-}
module Hare where
import Control.Monad
import Control.Applicative 
import HareMonad 

data RE :: * -> * where
  Empty  :: RE ()
  Fail   :: RE a
  Char   :: [Char] -> RE Char
  Seq    :: RE a -> RE b -> RE (a,b)
  Choose :: RE a -> RE a -> RE a
  Star   :: RE a -> RE [a]
  Action :: (a -> b) -> RE a -> RE b

match :: (Alternative f, Monad f) => RE a -> Hare f a
match Empty = pure ()
match Fail = failure
match (Char cs) = do
    x <- readCharacter
    guard (x `elem` cs)
    pure x
match (Seq a b) =  (,) <$> match a <*> match b
match (Choose a b) =  match a <|> match b
match (Star a) =
       addFront <$> match a <*> match (Star a)
     <|> pure []
   where 
     addFront x xs = (x:xs)
match (Action f a) = f <$> match a


matchAnywhere :: (Alternative f, Monad f) => RE a -> Hare f a
matchAnywhere re = match re <|> (readCharacter >> matchAnywhere re)

(=~) :: (Alternative f, Monad f) => String -> RE a -> f a 
(=~) = flip (hare . matchAnywhere)

infixr `cons`  
cons :: RE a -> RE [a] -> RE [a]
cons x xs = Action cons' $ Seq x xs
  where
    cons' :: (a,[a]) -> [a]
    cons' (x,xs) = x:xs


-- Uses Char, cons, Empty, Action
--   :[] used to map String to list of Strings, where each string is individual char
--   then use  cons . Char function composition to add to the empty list
--   empty list created using Action and Empty seen in test
string :: String -> RE String
string xs = foldr (cons . Char . (:[])) (Action (const []) Empty) xs

rpt :: Int -> RE a -> RE [a]
rpt n re | n < 0     = Action (const []) Empty
         | otherwise = foldr cons (Action (const []) Empty) (replicate n re)

rptRange :: (Int, Int) -> RE a -> RE [a]
rptRange (x,y) re = choose $ map (flip rpt re) (reverse [x..y]) 
--rptRange (x,y) re = foldr (Choose . (flip rpt re)) Fail (reverse [x..y])


option :: RE a -> RE (Maybe a)
option re = (Action Just re) `Choose` (Action (const Nothing) Empty)

plus :: RE a -> RE [a]
plus re = Action plus' $ Star re `Seq` re
  where
    plus' :: ([a],a) -> [a]
    plus' (xs,x) = xs ++ [x]

choose :: [RE a] -> RE a
choose res = foldr Choose Fail res
