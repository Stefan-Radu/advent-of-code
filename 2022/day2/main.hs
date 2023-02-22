import System.IO
import Data.Char (ord, chr)


decode1 :: Char -> Char
decode1 i 
    | a <= o && o <= c = i
    | x <= o && o <= z = chr $ a + o - x
    | otherwise = error "unsupported input"
    where x = ord 'X'
          o = ord i
          z = ord 'Z'
          a = ord 'A'
          c = ord 'C'


whatOutcome :: Char -> Outcome
whatOutcome c = case c of 'X' -> Lose
                          'Y' -> Draw
                          'Z' -> Win
                          otherwise -> Lose

whatMoveForOutcome :: Char -> Outcome -> Char
whatMoveForOutcome c Draw = c
whatMoveForOutcome c Lose = let r = (ord c) - 1
                                in if r >= ord 'A' then chr r else 'C'
whatMoveForOutcome c Win = let r = (ord c) + 1
                                in if r <= ord 'C' then chr r else 'A'

decode2 :: Char -> Char -> Char
decode2 i j 
     | a <= o && o <= c = j
     | x <= o && o <= z = whatMoveForOutcome i outcome
     | otherwise = error "unsupported input"
    where x = ord 'X'
          o = ord j
          z = ord 'Z'
          a = ord 'A'
          c = ord 'C'
          outcome = whatOutcome j

data Outcome = Win | Lose | Draw deriving Show

outcome :: Char -> Char -> Outcome
outcome 'A' 'B' = Win
outcome 'B' 'C' = Win
outcome 'C' 'A' = Win
outcome a b = if a == b then Draw else Lose

instance Eq Outcome where
    Win == Win = True
    Lose == Lose = True
    Draw == Draw = True
    _ == _ = False

moveScore :: Char -> Int
moveScore c = let a = ord 'A'
              in (ord c) - a + 1

outcomeScore :: Outcome -> Int
outcomeScore o = case o of Win -> 6
                           Draw -> 3
                           Lose -> 0

roundScore a b = (moveScore b) + (outcomeScore roundOutcome)
    where roundOutcome = outcome a b

parseLine :: String -> (Char, Char)
parseLine l = (head l, head $ drop 2 l)

main = do
    content <- readFile "input"
    let allLines = lines content
        parsed = map parseLine allLines
        --scores = [roundScore (fst x) (decode1 $ snd x) | x <- parsed]
        scores = [roundScore (fst x) (decode2 (fst x) (snd x)) | x <- parsed]
    return (sum scores)
