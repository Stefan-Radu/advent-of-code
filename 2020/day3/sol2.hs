module Main where

import System.IO

makeInt chr = if chr == '#' then 1 else 0

count string = let 
  li = [1 | x <- string, x == '#'] in
  length li

process [] _ _ _ _ = 0
process (h:t) j addJ i modulo = 
  let nextJ = (j + addJ) `mod` (length h) in
  let nextI = (i + 1) `mod` modulo in
  if i /= 0
     then process t j addJ nextI modulo
     else count [h !! j] + (process t nextJ addJ nextI modulo)

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  let 
    rezLi = map (\(a, b) -> process (lines contents) 0 a 0 b) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  print (foldr (*) 1 rezLi)
