import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Data.List(reverse)


rot13 :: String -> String
rot13 = map $ \x ->
    case lookup x table of
        Just y -> y
        Nothing -> x
    where
      table = table' 'A' 'Z' ++ table' 'a' 'z'
      table' a z = zip [a..z] (drop 13 (cycle [a..z]))


prop_length x = length x == length (rot13 x)
prop_map x = rot13 (map toUpper x) == map toUpper (rot13 x)
prop_map2 f x = rot13 (map f x) == map f (rot13 x)
prop_alpha x = all (not . isAlpha) x ==> rot13 x == x
prop_ab a b = rot13 (a ++ b) == rot13 a ++ rot13 b
prop_ord x = not (null x) ==> ord (head x) + 13 == ord (head (rot13 x))
prop_origin x = rot13 (rot13 x) == x



main = do
   quickCheck prop_length
   quickCheck prop_map
   quickCheck prop_map2
   quickCheck prop_alpha
   quickCheck prop_ab
   quickCheck prop_ord
   quickCheck prop_origin

