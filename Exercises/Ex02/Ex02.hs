module Ex02 where
import Test.QuickCheck
import Data.List
ones = 1 : ones
naturals = 0 : map (1+) naturals

-- implement the following functions, which meet some (but not all!) of the 
-- properties of a correct sorting function

-- prop2 & 4, but not prop1 & 3 & 5
dodgySort1 :: [Int] -> [Int]
dodgySort1 xs = xs 
--------------DONE------------ returns itself

-- prop1 & 2 & 3, but not prop4 & 5
dodgySort2 :: [Int] -> [Int]
dodgySort2 xs = insertionSort (xs ++ xs)
 --------------DONE------------ returns sorted of double list (satisfies all elements in list, same if it was reversed, and that in ascending order)

-- prop1 & 3 & 4, but not prop2 & 5
dodgySort3 :: [Int] -> [Int]
dodgySort3 xs = take (length xs) naturals
 --------------DONE------------ returns list of ascending natural numbers of length of given list

-- prop1 & 2 & 3 & 4, but not prop5
dodgySort4 :: [Int] -> [Int]
dodgySort4 xs = take (length xs - length ls) (map (*h) ones) ++ ls 
     where
        ls = insertionSort $ nub xs
        h = head ls
 --------------DONE------------ removes any duplicates from list (which will reduce the length of list) and append any number of lowest in list to front to compensate for reduced length

-- Properties of sorting function    
------Property 1: Sorted of list is same as sorted of reverse list
sortProp1 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp1 sortFn xs = sortFn xs == sortFn (reverse xs)

------Property 2: x is an element of sorted list, where x is after xs before ys
sortProp2 :: ([Int] -> [Int]) -> Int -> [Int] -> [Int] -> Bool
sortProp2 sortFn x xs ys = x `elem` sortFn (xs ++ [x] ++ ys)

------Property 3: purely sorted based on less than/equal to
sortProp3 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp3 sortFn xs = isSorted (sortFn xs)
  where 
    isSorted (x1 : x2 : xs) = (x1 <= x2) && isSorted (x2 : xs)
    isSorted _ = True

------Property 4: length of sorted list is same as original list
sortProp4 :: ([Int] -> [Int]) -> [Int] -> Bool    
sortProp4 sortFn xs = length xs == length (sortFn xs)

------Property 5: the sort function returns the same sortedlist as insertionsort (so it must be correctly sorted to pass)
sortProp5 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp5 sortFn xs 
  = sortFn xs == insertionSort xs

insertionSort :: [Int] -> [Int]
insertionSort xs = foldr insertSorted [] xs
  where 
    insertSorted x [] = [x]
    insertSorted x (y : ys) 
      | x <= y = x : y : ys
      | otherwise = y : insertSorted x ys

