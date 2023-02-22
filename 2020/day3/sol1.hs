module Main where

import System.IO

makeInt chr = if chr == '#' then 1 else 0

count string = let 
  li = [1 | x <- string, x == '#'] in
  length li

process [] _ = 0
process (h:t) index = 
  let nextIndex = (index + 3) `mod` (length h) in
  count [h !! index] + (process t nextIndex)

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  print (process (lines contents) 0)
