module Main where

import System.IO

splitAux::[Char] -> ([[Char]], Bool)
splitAux [] = ([], False)
splitAux [chr] = ([[chr]], False)
splitAux (h1:h2:rest)
  | sep = (fst (splitAux rest), True)
  | wasSplit = ([h1]:result, False)
  | otherwise = ((h1:(head result)):(tail result), False)
  where (result, wasSplit) = splitAux (h2:rest)
        sep = (h1 == h2) && (h1 == '\n')

split li = do
  let map1 li' = map (\char -> if char == '\n' then ' ' else char) li'
      map2 li' = map (\li'' -> map1 li'') li'
   in return (map2 (fst (splitAux li)))

onePerLine [] = return ()
onePerLine (h:t) = do
  putStrLn h
  putChar '\n'
  onePerLine t

inList x li = (filter (\elem -> elem == x) li) /= []

checkKeys li = 
  let bestLi = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
      filtered = filter (\elem -> inList elem li) bestLi
   in if filtered == bestLi then 1 else 0

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  return contents
  let (a, b) = splitAux contents
  splitContents <- split contents
  let splitWords = map words splitContents
      splitKeys = [map (\word -> take 3 word) li | li <- splitWords]
      results = map (\li -> checkKeys li) splitKeys

  print (sum results)
