{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module A2 where

import A1
import Data.List (intercalate)
import Data.Char (digitToInt)

-- *** Assignment 2-1 *** --

-- Q#01
promptPlayer :: Player -> [Char]
promptPlayer h = concat ["Player ", show h, "'s turn: enter a row and column position (ex. A1)"]

-- Q#02
_RANGE_ :: [Int]
_RANGE_ = [0 .. (_SIZE_ - 1)]

-- Q#03
isDigit :: Char -> Bool
isDigit i = elem i ['0' .. '9']

readDigit :: Char -> Int
readDigit j = if elem j ['0' .. '9'] then read (show (digitToInt j)) else -1

-- Q#04
_EMPTY_ROW_ :: Row
_EMPTY_ROW_ = replicate _SIZE_ E

_EMPTY_BOARD_ :: Board
_EMPTY_BOARD_ = replicate _SIZE_ (replicate _SIZE_ E)

-- Q#05
isTied :: Board -> Bool
isTied bord = notElem E (concat bord)

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
        [X, O, O]
      , [O, X, X]
      , [O, X, O]
      ]

-- Q#06
_ABC_ :: [Char]
_ABC_ = ['A' ..]

indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings o = zip _ABC_ o

-- Q#07
formatLine :: [String] -> String
formatLine p = _SEP_ ++ (intercalate _SEP_ p) ++ _SEP_

-- *** Assignment 2-2 *** --

-- Q#08
isMoveInBounds :: Move -> Bool
isMoveInBounds (w, v)
  | w > (_SIZE_ - 1) = False
  | v > (_SIZE_ - 1) = False
  | w < 0 = False
  | v < 0 = False
  | otherwise = True

-- Q#09
stringToMove :: String -> Move
stringToMove zz = if (length zz /= 2) then _INVALID_MOVE_ else (((digitToInt (head zz))-10), (digitToInt (last zz)))
 

-- Q#10
replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow aa bb cc 
 | bb == 0 = aa : (tail cc)
 | bb == 1 = (head cc) : aa : snd(splitAt 2 cc)
 | bb == 2 = (head cc) : (head(tail cc)) : aa : []
 | otherwise = cc

rsX :: Int -> Row -> Row
rsX dd ee = replaceSquareInRow X dd ee

rsO :: Int -> Row -> Row
rsO ff gg = replaceSquareInRow O ff gg



