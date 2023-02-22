import System.IO
import Data.Char (ord, chr)

charToDigit :: Char -> Int
charToDigit c = ord c - ord '0'

stringToInt :: String -> Int
stringToInt s = let aux x "" = x
                    aux x (h:t) = aux (x * 10 + charToDigit h) t
                in aux 0 s

collapseValues :: [String] -> [Int]
collapseValues [] = []
collapseValues [x] = [stringToInt x]
collapseValues (h:t) 
    | h == "" = 0:(collapseValues t)
    | otherwise = ((head res) + (stringToInt h)):(tail res)
    where res = collapseValues t


getMaxValue :: [Int] -> Maybe Int
getMaxValue [] = Nothing
getMaxValue [x] = Just x
getMaxValue (h:t) = let res = getMaxValue t
                    in case res of Nothing -> Nothing
                                   Just k -> if h > k then Just h else Just k

fromMaybe :: Maybe Int -> Int
fromMaybe Nothing = 0
fromMaybe (Just k) = k

merge :: [Int] -> [Int] -> [Int]
merge x [] = x
merge [] x = x
merge (h1:t1) (h2:t2) 
    | h1 < h2 = h1:(merge t1 (h2:t2))
    | otherwise = h2:(merge (h1:t1) t2)

mergeSort :: [Int] -> [Int]
mergeSort li 
    | length li <= 1 = li
    | otherwise = let mid = (length li) `div` 2
                      leftRes = mergeSort $ take mid li
                      rightRes = mergeSort $ drop mid li
                   in merge leftRes rightRes

task1 allLines = let sums = collapseValues allLines
                  in fromMaybe $ getMaxValue sums

task2 allLines = let sortedSums = mergeSort $ collapseValues allLines
                     howManyDrops = (length sortedSums) - 3
                     afterDrop = drop howManyDrops sortedSums
                  in sum afterDrop

main = do
    contents <- readFile "input"
    let allLines = lines contents
        --value = task1 allLines
        value = task2 allLines
    return value
