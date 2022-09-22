module Ex05 where

import Text.Read (readMaybe)

data Token = Number Int | Operator (Int -> Int -> Int)

parseToken :: String -> Maybe Token
parseToken "+" = Just (Operator (+))
parseToken "-" = Just (Operator (-))
parseToken "/" = Just (Operator div)
parseToken "*" = Just (Operator (*))
parseToken str = fmap Number (readMaybe str)

tokenise :: String -> Maybe [Token]
tokenise = mapM parseToken . words
-- equivalent to
-- tokenise str = mapM parseToken $ words str

newtype Calc a = C ([Int] -> Maybe ([Int], a))


pop :: Calc Int
pop  = C pop'
  where
    pop' :: [Int] -> Maybe ([Int], Int)
    pop' [] = Nothing
    pop' (x:xs) = Just (xs, x)
-- I submitted with pure (xs,x) but Just is cleaner


push :: Int -> Calc ()
push i = C $ push' i
  where
    push' :: Int -> [Int] -> Maybe ([Int], ())
    push' x xs = Just ((x:xs), ())
-- also had pure here too but Just is better for clarity




instance Functor Calc where
  fmap f (C sa) = C $ \s ->
      case sa s of 
        Nothing      -> Nothing
        Just (s', a) -> Just (s', f a)

instance Applicative Calc where
  pure x = C (\s -> Just (s,x))
  C sf <*> C sx = C $ \s -> 
      case sf s of 
          Nothing     -> Nothing
          Just (s',f) -> case sx s' of
              Nothing      -> Nothing
              Just (s'',x) -> Just (s'', f x)

instance Monad Calc where
  return = pure
  C sa >>= f = C $ \s -> 
      case sa s of 
          Nothing     -> Nothing
          Just (s',a) -> unwrapCalc (f a) s'
    where unwrapCalc (C a) = a

evaluate :: [Token] -> Calc Int
-- evaluate will go over each Token, and for each Token
-- if Number, push to Stack
-- if Operator, pull two from stack, eval with operator
--   then push to stack
-- if Token empty, pure the first element in the stack
evaluate [] = pop
evaluate (t:ts) = do 
    case t of
      Number   i -> push i
      Operator o -> do
          y <- pop
          x <- pop
          push (o x y)
    evaluate ts


--evaluate ((Number   i) :ts) = do
--  push i
--  evaluate ts
--evaluate ((Operator o) :ts) = do
--  y <- pop
--  x <- pop
--  push (o x y)
--  evaluate ts


calculate :: String -> Maybe Int
calculate s = do
  case evaluate <$> tokenise s of
    Just (C f) -> snd <$> f []


-- Below commented is the equivalent for calculate, 
-- but realised a better and cleaner implementation
{-
calculate s = do
    ts <- tokenise s
    let (C f) = evaluate ts
    (st,x) <- f []
    pure x
-}
