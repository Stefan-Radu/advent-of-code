module Main where

import System.IO
import Data.Bits

getRow string = let list = zip string [6, 5..0]
                    bits = [if x == 'B' then (1 `shiftL` y) :: Int else 0 | (x, y) <- list]
                 in sum bits

getColumn string = let list = zip string [2, 1, 0]
                       bits = [if x == 'R' then (1 `shiftL` y) :: Int else 0 | (x, y) <- list]
                    in sum bits

getSeatId line = do
  let x = take 7 line
      y = drop 7 line
  return ((getRow x) * 8 + (getColumn y))


solve handle = do
  isEOF <- hIsEOF handle
  if isEOF
     then return []
     else do
       line <- hGetLine handle
       seatId <- getSeatId line
       semiBest <- solve handle
       return (seatId:semiBest)

xorSum list = (foldr (\accum x -> accum `xor` x) 0 list) :: Int

getMin [] = 10 ^ 10
getMin (h:t) = min h (getMin t)

getMax [] = 0
getMax (h:t) = max h (getMax t)

sort :: [Int] -> [Int]
sort [] = [] 
sort (h:t) = 
  let left = [x | x <- (h:t), x < h]
      right = [x | x <- (h:t), x > h]
   in (sort left) ++ [h] ++ (sort right)

main = do
  handle <- openFile "input" ReadMode
  ids <- solve handle
  let minimum = getMin ids
      maximum = getMax ids
      idsFull = [minimum..maximum]
      fstSum = xorSum idsFull
      sndSum = xorSum ids

  print (fstSum `xor` sndSum)
  hClose handle
