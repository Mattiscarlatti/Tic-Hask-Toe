module A1 where

import Data.Char (toUpper)

-- *** Assignment 1-1 *** --

-- Q#01
_SIZE_ :: Int
_SIZE_ = 3

-- Q#02
_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True

-- Q#03
convertRowIndex :: Char -> Int
convertRowIndex x = fromEnum (toUpper x) - 65

-- Q#04
_INVALID_MOVE_ :: (Int, Int)
_INVALID_MOVE_ = (-1, -1)

-- Q#05
_SEP_ :: [Char]
_SEP_ = ['_', '|', '_']

-- *** Assignment 1-2 *** --

-- Q#06
data Square = X | O | E deriving (Show, Read, Eq, Ord)

-- Q#07
data GameState = Xwon | Owon | Tie | InProgress deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- Q#08
type Line = [Square]
type Row = [Square]
type Board = [Row]
type Player = Square
type Move = (Int, Int)

-- Q#09
getFirstPlayer :: Bool -> Square
getFirstPlayer y = if (y == True) then X else O

getFirstPlayer_ :: Bool -> Square
getFirstPlayer_ z
  | z == True = X
  | z == False = O

-- Q#10
showGameState :: GameState -> [Char]
showGameState gs = case gs of
  Xwon -> "Player X won big time!"
  Owon -> "Player O obliverated his opponent!"
  Tie -> "It's a Tie"
  InProgress -> "The Game is not over yet..."

-- Q#11
switchPlayer :: Player -> Player
switchPlayer played = case played of 
    X -> O 
    O -> X
    E -> E

-- Q#12
showSquare :: Square -> [Char]
showSquare fieldCode 
  | fieldCode == X = "X"
  | fieldCode == O = "O"
  | fieldCode == E = "_"