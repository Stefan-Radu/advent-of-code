import System.IO
import Data.Char (ord)
import Data.Set (toList, fromList)

unique lst = toList $ fromList lst

group3 :: [String] -> [[String]]
group3 [] = []
group3 [x] = error "bad"
group3 [x, y] = error "bad"
group3 (x:y:z:t) = [x, y, z]:(group3 t)

getHalves :: [Char] -> ([Char], [Char])
getHalves li = 
    let splitPoint = (length li) `div` 2
        split = splitAt splitPoint li
        a = unique $ fst split
        b = unique $ snd split
     in (a, b)

-- is x inside of list li?
contains :: (Foldable t, Eq a) => a -> t a -> Bool
contains x li = let found = foldl (\acc k -> if k == x then k:acc else acc) [] li
                    len = length found
                 in len /= 0

-- common characters between the two lists
getCommon2 :: [Char] -> [Char] -> [Char]
getCommon2 l1 l2 = foldl (\acc k -> if (contains k l2) then k:acc else acc) [] l1

getCommon :: [String] -> String
getCommon [] = []
getCommon [x] = x
getCommon li = let [h1, h2] = take 2 li
                   aux = getCommon2 h1 h2
                   tail = drop 2 li
                in getCommon (aux:tail)

decode :: Char -> Int
decode x
    | ord 'a' <= val && val <= ord 'z' = ord x - ord 'a' + 1
    | ord 'A' <= val && val <= ord 'Z' = ord x - ord 'A' + 27
    | otherwise = 0
    where val = ord x

solveForOne :: String -> Int
solveForOne str =
    let halves = getHalves str
        a = fst halves
        b = snd halves
        common = getCommon [a, b]
        decoded = map decode common
    in sum decoded

solve1 :: [String] -> Int
solve1 input = 
    let applied = map solveForOne input
    in sum applied

solve2 :: [String] -> Int
solve2 input = 
    let groups = group3 input
        common = map getCommon groups
        decoded = map (decode . head) common
    in sum decoded

main = do
    content <- readFile "input"
    --putStr content
    let allLines = lines content
        s1 = solve1 allLines
        s2 = solve2 allLines
    return (s1, s2)
