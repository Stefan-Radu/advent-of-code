
getRange :: String -> (Int, Int)
getRange rangeStr = let split = break (== '-') rangeStr
                        f = read $ fst split :: Int
                        s = read $ tail $ snd split :: Int
                     in (f, s)

getRanges :: String -> ((Int, Int), (Int, Int))
getRanges line = let split = break (== ',') line
                     f = fst split
                     s = tail $ snd split
                 in (getRange f, getRange s)

contained :: ((Int, Int), (Int, Int)) -> Int
contained ((a, b), (c, d)) 
    | a <= c && d <= b = 1
    | c <= a && b <= d = 1
    | otherwise = 0

overlapped :: ((Int, Int), (Int, Int)) -> Int
overlapped ((a, b), (c, d)) 
    | b < c = 0
    | d < a = 0
    | otherwise = 1

solve1 :: [String] -> Int
solve1 input = let ranges = map getRanges input
                in sum $ map contained ranges

solve2 :: [String] -> Int
solve2 input = let ranges = map getRanges input
                in sum $ map overlapped ranges

main = do
    fileContent <- readFile "input"
    let allLines = lines fileContent
        s1 = solve1 allLines
        s2 = solve2 allLines
    return (s1, s2)
