module Main where

import System.IO
import Data.Char

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

isNumber' :: [Char] -> Bool
isNumber' string = ((filter isDigit string) == string) && head string /= '0'

checkBYR string =
  let isN = isNumber' string
   in if not isN
     then 1
     else let x = read string
           in (if (1920 <= x && x <= 2002) then 0 else 1)

checkIYR string =
  let isN = isNumber' string
   in if not isN
     then 1
     else let x = read string
           in (if (2010 <= x && x <= 2020) then 0 else 1)

checkEYR string =
  let isN = isNumber' string
   in if not isN
     then 1
     else let x = read string
           in (if (2020 <= x && x <= 2030) then 0 else 1)

checkHGT string =
  if (length string) <= 2
     then 1
     else let dropAm = (length string) - 2
              x = take dropAm string
              y = drop dropAm string
              isN = isNumber' x
          in if (not isN)
            then 1
            else if y /= "cm" && y /= "in"
            then 1
            else let xx = read x
                  in if y == "cm"
                        then (if (150 <= xx && xx <= 193) then 0 else 1)
                        else (if (59 <= xx && xx <= 76) then 0 else 1)


isHex char = (filter (\chr -> chr == char) (['a'..'f'] ++ ['0'..'9'])) == [char]

checkHCL string = 
  let x = head string
      y = drop 1 string
      isN = (filter (\char -> not (isHex char)) y) == []
   in if not isN
     then 1
     else (if (x == '#') then 0 else 1)

checkECL string =
  let res = filter (\elem -> elem == string) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
   in if res == []
         then 1
         else if (head res) == string
         then 0
         else 1

checkPID string =
  let res = filter (\chr -> isDigit chr) string
   in (if (((length string) == 9) && (res == string)) then 0 else 1)

isWrong :: ([Char], [Char]) -> Int
isWrong (key, value) =
  let x = case key of "byr" -> checkBYR value
                      "iyr" -> checkIYR value
                      "eyr" -> checkEYR value
                      "hgt" -> checkHGT value
                      "hcl" -> checkHCL value
                      "ecl" -> checkECL value
                      "pid" -> checkPID value
                      "cid" -> 0
                      _ -> 1
   in x

inList x li = (filter (\(elem, _) -> elem == x) li) /= []

checkKeys :: [([Char], [Char])] -> Int
checkKeys li = 
  let bestLi = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
      filtered = filter (\elem -> inList elem li) bestLi
   in if filtered == bestLi
         then if (sum (map (\elem -> isWrong elem) li) /= 0)
                 then 0
                 else 1
         else 0

printDetails [] = do
  return ()

printDetails ((key, value):t) = do
  let x = case key of "byr" -> checkBYR value
                      "iyr" -> checkIYR value
                      "eyr" -> checkEYR value
                      "hgt" -> checkHGT value
                      "hcl" -> checkHCL value
                      "ecl" -> checkECL value
                      "pid" -> checkPID value
                      "cid" -> 0
                      _ -> 1

  print (key ++ " " ++ value ++ " " ++ (show x))
  printDetails t

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  return contents
  let (a, b) = splitAux contents
  splitContents <- split contents
  let splitWords = map words splitContents
      splitKeys = [map (\word -> (take 3 word, drop 4 word)) li | li <- splitWords]
      result = [checkKeys li | li <- splitKeys]

  printDetails (head splitKeys)
  print (sum result)
