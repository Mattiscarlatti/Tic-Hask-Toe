module A4 where

import A1
import A2
import A3 hiding (
  _HEADER_,
  showSquares,
  dropFirstCol,
  dropLastCol,
  formatRows,
  isWinningLine,
  prependRowIndices
  )

-- *** Assignment 4-1 *** --

-- Q#01
_HEADER_ :: String
_HEADER_ = " " ++ formatLine (map (\x -> show x) _RANGE_)

-- Q#02
showSquares :: [Square] -> [String]
showSquares ss = map (\x -> showSquare x) ss

-- Q#03
dropFirstCol :: Board -> Board
dropFirstCol tt = map (\x -> snd(splitAt 1 x)) tt

-- Q#04
dropLastCol :: Board -> Board
dropLastCol uu = map (\x -> fst(splitAt ((length x) - 1) x)) uu

--Q#05
formatRows :: [Row] -> [String]
formatRows vv = map (\x -> formatLine (showSquares x)) vv

-- Q#06
isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ ww [] = False
isWinningLine_ ww xx = all (== True) (map (\x -> (x == ww)) xx)


-- *** Assignment 4-2 *** --

-- Q#07
isWinningLine :: Player -> Line -> Bool
isWinningLine yy zz = foldr (\x b -> x == yy && b) (if zz == [] then False else True) zz

-- Q#08
hasWon :: Player -> Board -> Bool
hasWon aaa bbb = foldr (\x b -> (isWinningLine aaa x) || b) False (getAllLines bbb)
--hasWon aaa bbb = any (== True) (map (\x -> (isWinningLine aaa x)) (getAllLines bbb))

-- Q#09
getGameState :: Board -> GameState
getGameState ccc
 | (hasWon X ccc == True) = Xwon
 | (hasWon O ccc == True) = Owon
 | (isTied ccc == True) = Tie
 | otherwise = InProgress

playMove :: Player -> Board -> Move -> (GameState, Board)
playMove ddd eee fff = (getGameState (newBoard), (newBoard))
 where newBoard = putSquare ddd eee fff

-- Q#10
prependRowIndices :: [String] -> [String]
prependRowIndices ggg = zipWith (:) ['A' ..] ggg

-- Q#11
formatBoard :: Board -> String
formatBoard hhh = unlines . (_HEADER_ :) . prependRowIndices . formatRows $ hhh