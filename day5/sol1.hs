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
     then return (-1)
     else do
       line <- hGetLine handle
       seatId <- getSeatId line
       semiBest <- solve handle
       return (max seatId semiBest)

main = do
  handle <- openFile "input" ReadMode
  ans <- solve handle
  print ans
  hClose handle
