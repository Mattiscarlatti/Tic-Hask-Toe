module A3 where

import A1
import A2

import Data.List (transpose)
import Data.Maybe 

-- *** Assignment 3-1 ***

-- Q#01
showInts :: [Int] -> [String]
showInts [] = []
showInts (x : xs) = (show x) : showInts xs


_HEADER_ = " " ++ formatLine (showInts _RANGE_)

-- Q#02
showSquares :: [Square] -> [String]
showSquares [] = []
showSquares (x : xs) = (showSquare x) : showSquares xs


-- Q#03
formatRows :: [Row] -> [String]
formatRows [] = []
formatRows (x : xs) = (formatLine (showSquares x)) : formatRows xs

-- Q#04
isColEmpty :: Row -> Int -> Bool
isColEmpty [] _ = False
isColEmpty (x : xs) (-1) = False
isColEmpty (x : xs) hh = if ((hh == 0) && (x == E)) then True else (isColEmpty xs (hh - 1))


-- Q#05
dropFirstCol :: Board -> Board
dropFirstCol [] = []
dropFirstCol (x : xs) = snd(splitAt 1 x) : dropFirstCol xs

dropLastCol :: Board -> Board
dropLastCol [] = []
dropLastCol (x : xs) = fst(splitAt ((length x) - 1) x) : dropLastCol xs

-- Q#06
getDiag1 :: Board -> Line
getDiag1 [] = []
getDiag1 (x : xs) = (head x) : getDiag1 (dropFirstCol xs)
    

getDiag2 :: Board -> Line
getDiag2 [] = []
getDiag2 (x : xs) = (last x) : getDiag2 (dropLastCol xs)

getAllLines :: Board -> [Line]
getAllLines jj = concat [(transpose jj), (getDiag1 jj) : (getDiag2 jj) : jj]

-- *** Assignment 3-2 ***

-- Q#07
putSquare :: Player -> Board -> Move -> Board
putSquare kk ll mm 
 | ll == [] = []
 | (fst(mm)) == 0 = (replaceSquareInRow kk (snd(mm)) (head(ll))) : (tail(ll))
 | (fst(mm)) == 1 = (head(ll)) : (replaceSquareInRow kk (snd(mm)) (head(tail ll))) : (last(ll)) : []
 | (fst(mm)) == 2 = (head(ll)) : (head(tail ll)) : (replaceSquareInRow kk (snd(mm)) (last(ll))) : []
 | otherwise = ll
 

-- Q#08
prependRowIndices :: [String] -> [String]
prependRowIndices nn = go (indexRowStrings nn) where
    go :: [(Char, String)] -> [String]
    go [] = []
    go (x:xs) = (fst(x) : snd(x)) : go (xs)

-- Q#09
isWinningLine :: Player -> Line -> Bool
isWinningLine oo pp = go False oo pp where
    go :: Bool -> Player -> Line -> Bool
    go bool oo [] = bool
    go _ oo (x:xs) = x == oo && go True oo xs

-- | pp == [] = False
-- | otherwise = if ((head(pp) == oo) && (head(tail(pp)) == oo) && (last(pp) == oo)) then True else False

-- Q#10
isValidMove :: Board -> Move -> Bool
isValidMove [] rr = False
isValidMove qq rr = (isMoveInBounds rr == True) && (((qq !! (fst(rr))) !! (snd(rr))) == E)

{--isValidMove qq rr = if (isMoveInBounds rr == True) then case rr of
   (0, 0) -> if (head(head(qq)) == E) then True else False
   (0, 1) -> if (head(tail(head(qq))) == E) then True else False
   (0, 2) -> if (last(head(qq)) == E) then True else False
   (1, 0) -> if (head(head(tail(qq))) == E) then True else False
   (1, 1) -> if (head(tail(head(tail(qq)))) == E) then True else False
   (1, 2) -> if (last(head(tail(qq))) == E) then True else False
   (2, 0) -> if (head(last(qq)) == E) then True else False
   (2, 1) -> if (head(tail(last(qq))) == E) then True else False
   (2, 2) -> if (last(last(qq)) == E) then True else False
 else False
 --}