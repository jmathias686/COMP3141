import Data.Char(toLower)
import Data.List(group, sort, sortBy)


breakIntoWords :: String -> [String]
breakIntoWords = words

convertIntoLowercase :: [String] -> [String]
convertIntoLowercase = map (map toLower) -- [String] == [[Char]]

sortWords :: [String] -> [String]
sortWords = sort

type Run = (Int, String)
countAdjacentRuns :: [String] -> [Run]
countAdjacentRuns = convertToRuns . groupAdjacentRuns 


groupAdjacentRuns :: [String] -> [[String]]
groupAdjacentRuns = group

convertToRuns :: [[String]] -> [Run]
convertToRuns = map (\ls -> (length ls, head ls))


sortByRunSize :: [Run] -> [Run]
sortByRunSize = sortBy (\(l1, w1) (l2, w2) -> compare l2 l1)

takeFirst :: Int -> [Run] -> [Run]
takeFirst = take


generateReport :: [Run] -> String
generateReport = unlines . map (\(l,w) -> w ++ ":" ++ show l )

mostCommonWords :: Int -> String -> String
mostCommonWords n = 
     generateReport
   . takeFirst n
   . sortByRunSize
   . countAdjacentRuns    
   . sortWords
   . convertIntoLowercase
   . breakIntoWords



