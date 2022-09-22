Module Dictionary
    ( Word
    , Definition
    , Dict -- can export multiple constructurs using (..), or single ones like (D)
    , emptyDict
    , insertWord
    , lookup
    )
where


import Prelude hiding (Word, lookup)
import qualified Prelude
import Test.QuickCheck

-- lookup :: Eq a => a -> [(a,b)] -> Maybe b
type Word = String
type Definition = String

newtype Dict = D [DictEntry] deriving (Show, Eq)

emptyDict :: Dict
emptyDict = D []

insertWord :: Word -> Definition -> Dict -> Dict
insertWord w d (D entries) = D (insertSorted (Entry w d) entries)
  where
    insertSorted :: Ord a => a -> [a] -> [a]
    insertSorted e [] = [e]
    insertSorted e (x:xs) | e <= x    = e : x : xs
                          | otherwise = x : insertSorted e xs



lookup :: Word -> Dict -> Maybe Definition
lookup w (D es) = lookupSorted w es
  where
    lookupSorted :: Word -> [DictEntry] -> Maybe Definition
    lookupSorted w [] = Nothing
    lookupSorted w (e:es) = case compare w (word e) of
        LT -> Nothing
        EQ -> Just (defn e)
        GT -> lookupSorted w es

lookup' :: Word -> Dict -> Maybe Definition
lookup' w (D es) = Prelude.lookup w (map (\(Entry w d) -> (w,d)) es)



data DictEntry = Entry { word :: Word
                       , defn :: Definition
                       } deriving (Eq, Show)


prop_emptyDict_wf = wf emptyDict

prop_insertWord_wf w d dict = wf dict ==> wf (insertWord w d dict)

prop_lookup_lookup' w d = wf d ==> lookup w d == lookup' w d


wf :: Dict -> Bool
wf (D es) = sorted es
  where
    sorted :: (Ord a) => [a] -> Bool
    sorted [] = True
    sorted [x] = True
    sorted (x:y:xs)
      | x <= y    = sorted (y : xs)
      | otherwise = False


instance Ord DictEntry where
  Entry w1 d1 <= Entry w2 d2 = w1 <= w2

instance Arbitrary DictEntry where
  arbitrary = Entry <$> arbitrary <*> arbitrary

instance Arbitrary Dict where
  arbitrary = do
    Ordered ds <- arbitrary
    pure (D ds)
