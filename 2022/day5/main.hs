split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

takeStacks :: [String] -> [String]
takeStacks [] = []
takeStacks (h:t)
    | h == "" = []
    | otherwise = h:(takeStacks t)

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [a] = []
dropLast (h:t) = h:(dropLast t)

parse :: String -> String
parse "" = ""
parse s = let h = take 4 s
              mid = head $ tail h
              rest = drop 4 s
           in mid:(parse rest)

--takeFistPutSeparate :: [String] -> String
buildStackContainer :: Int -> [[Char]]
buildStackContainer 0 = []
buildStackContainer k = []:(buildStackContainer (k - 1))

takeFirstOfEach :: [String] -> String
takeFirstOfEach [] = ""
takeFirstOfEach (h:t) 
    | h == "" = rest
    | otherwise = if h' /= ' ' then h':rest else rest
    where h' = head h
          rest = takeFirstOfEach t

dropFirstOfEach :: [String] -> [String]
dropFirstOfEach [] = []
dropFirstOfEach (h:t) 
    | h == "" = rest
    | otherwise = t':rest
    where t' = tail h
          rest = dropFirstOfEach t

makeStacks :: [String] -> [String]
makeStacks [] = []
makeStacks li = let took = takeFirstOfEach li
                    dropped = dropFirstOfEach li
                    rest = makeStacks dropped
                 in took:rest

parseMove :: String -> (Int, Int, Int)
parseMove s = let spl = split ' ' s
                  a = read (head $ drop 1 spl) :: Int
                  b = read (head $ drop 3 spl) :: Int
                  c = read (head $ drop 5 spl) :: Int
               in (a, b, c)

makeMove :: [String] -> (Int, Int, Int) -> [String]
makeMove stacks (hm, i', j') = let i = i' - 1
                                   j = j' - 1
                                   a = take i stacks
                                   b = stacks !! i
                                   what' = take hm b
                                   what = reverse what'
                                   b' = drop hm b
                                   c = drop (i + 1) stacks
                                   took = a ++ (b':c)
                                   x = take j took
                                   y' = what ++ (took !! j)
                                   z = drop (j + 1) took
                                in x ++ (y':z)

makeMove2 :: [String] -> (Int, Int, Int) -> [String]
makeMove2 stacks (hm, i', j') = let i = i' - 1
                                    j = j' - 1
                                    a = take i stacks
                                    b = stacks !! i
                                    what = take hm b
                                    b' = drop hm b
                                    c = drop (i + 1) stacks
                                    took = a ++ (b':c)
                                    x = take j took
                                    y' = what ++ (took !! j)
                                    z = drop (j + 1) took
                                in  x ++ (y':z)

makeMoves :: [String] -> [(Int, Int, Int)] -> [String]
makeMoves stacks [] = stacks
makeMoves stacks (h:t) = let rest = makeMoves stacks t
                          in makeMove rest h 

makeMoves2 :: [String] -> [(Int, Int, Int)] -> [String]
makeMoves2 stacks [] = stacks
makeMoves2 stacks (h:t) = let rest = makeMoves2 stacks t
                          in makeMove2 rest h 

solve1 :: [String] -> [(Int, Int, Int)] -> String
solve1 stacks moves = let finalStacks' = makeMoves stacks (reverse moves)
                          l = length finalStacks'
                          finalStacks = take (l - 1) finalStacks'
                       in map head finalStacks

solve2 :: [String] -> [(Int, Int, Int)] -> String
solve2 stacks moves = let finalStacks' = makeMoves2 stacks (reverse moves)
                          l = length finalStacks'
                          finalStacks = take (l - 1) finalStacks'
                       in map head finalStacks
main = do
    fileContent <- readFile "input"
    let allLines = lines fileContent
        stacks = dropLast $ takeStacks allLines
        restOfInput = drop ((length stacks) + 2) allLines
        parsedRestOfInput = map parseMove restOfInput
        parsedStacks = map parse stacks
        usableStacks = makeStacks parsedStacks
        s1 = solve1 usableStacks parsedRestOfInput
        s2 = solve2 usableStacks parsedRestOfInput
        moved = makeMoves usableStacks (reverse parsedRestOfInput)
    return (s1, s2)
