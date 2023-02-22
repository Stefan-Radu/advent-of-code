import qualified Data.Set as Set

solve :: String -> Int -> Int -> Int
solve s d t
    | d + t > l = -1
    | otherwise = if (length uniq) == t then (d + t) else solve s (d + 1) t
    where l = length s
          section = take t (drop d s)
          uniq = Set.toList (Set.fromList section)

main = do
    content <- readFile "input"
    let s1 = solve content 0 4
    let s2 = solve content 0 14
    return (s1, s2)
