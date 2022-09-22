{-# LANGUAGE GADTs #-}

module Ex06 where

-- Datatype of formulas
-- --------------------

data Formula ts where
  Body   :: Term Bool                     -> Formula ()
  Exists :: Show a 
         => [a] -> (Term a -> Formula as) -> Formula (a, as)

data Term t where
  Name    :: String -> Term t    -- to facilitate pretty printing only. 
                                 -- don't use this in actual formulae.

  Con     :: t -> Term t -- Constant values

  -- Logical operators
  And     :: Term Bool -> Term Bool -> Term Bool
  Or      :: Term Bool -> Term Bool -> Term Bool

  -- Comparison operators
  Smaller :: Term Int  -> Term Int  -> Term Bool

  -- Arithmetic operators
  Plus    :: Term Int  -> Term Int  -> Term Int


-- Pretty printing formulas
-- ------------------------

instance Show t => Show (Term t) where
  show (Con v)       = show v
  show (And p q)     = "(" ++ show p ++ " && " ++ show q ++ ")"
  show (Or p q)      = "(" ++ show p ++ " || " ++ show q ++ ")"
  show (Smaller n m) = "(" ++ show n ++ " < "  ++ show m ++ ")"
  show (Plus n m)    = "(" ++ show n ++ " + "  ++ show m ++ ")"
  show (Name name)   = name

instance Show (Formula ts) where
  show = show' ['x' : show i | i <- [0..]]
    where
      show' :: [String] -> Formula ts' -> String
      show' ns     (Body body)   = show body
      show' (n:ns) (Exists vs p) = "exists " ++ n ++ "::" ++ show vs ++ ". " ++ show' ns (p (Name n))


-- Example formulas
-- ----------------

ex1 :: Formula ()
ex1 = Body (Con True)

ex2 :: Formula (Int, ())
ex2 = Exists [1..10] $ \n ->
        Body $ n `Smaller` (n `Plus` Con 1)

ex3 :: Formula (Bool, (Int, ()))
ex3 = Exists [False, True] $ \p -> 
      Exists [0..2] $ \n -> 
        Body $ p `Or` (Con 0 `Smaller` n)

ex4 :: Formula (Int, (Int, (Int, ())))
ex4 = Exists [2,4..10] $ \n ->
      Exists [(eval n+1)*2..22] $ \p ->
      Exists [1..4] $ \o ->
        Body $ (o `Smaller` p) `And` ((n `Plus` o) `Smaller` p)

-- Evaluating terms
-- ----------------
eval :: Term t -> t
eval (Con v)       = v
eval (And p q)     = eval p && eval q
eval (Or p q)      = eval p || eval q
eval (Smaller n m) = eval n < eval m
eval (Plus n m)    = eval n + eval m

-- the Name constructor is not relevant for evaluation
-- just throw an error if it is encountered:
eval (Name _) = error "eval: Name"   


-- Checking formulas
-- -----------------
-- For each Body t, must eval t as its just a Term.
-- For each Exists, map Con to make each element a Term element
--   then apply the combined function of f (to return a formula) and
--   satisfiable from the returned formula to any to return a Bool
satisfiable :: Formula ts -> Bool
satisfiable (Body t) = eval t
satisfiable (Exists xs f) = any (satisfiable . f) $ map Con xs

-- Enumerating solutions of formulae
-- ---------------------------------
-- need to filter
solutions :: Formula ts -> [ts]
solutions (Body t) = [()]
solutions (Exists [] _) = []
solutions (Exists (x:xs) f) = 
  [(i,j) | i <- if satisfiable $ f (Con x) then [x] else [],
           j <- solutions $ f (Con x) ]
  ++ solutions (Exists xs f)
  
