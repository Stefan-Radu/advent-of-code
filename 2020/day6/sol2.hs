module Main where

import System.IO
import Data.Set

solve handle = do
  isEOF <- hIsEOF handle
  if isEOF
     then return ([fromList ['a'..'z']], False)
     else do
       line <- hGetLine handle
       (resp, newGroup) <- solve handle
       if line == ""
          then return (resp, True)
          else if newGroup
          then return ((fromList line):resp, False)
          else return ((intersection (fromList line) (head resp)):(tail resp), False)

getLengths sets = Prelude.map (\s -> size s) sets

main = do
  handle <- openFile "input" ReadMode
  (sets, _) <- solve handle
  let lengths = getLengths sets
  print (sum lengths)
  hClose handle
