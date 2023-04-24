module Main where

import A1
import A2
import A3
import A4
import A5

rrr :: Player -> IO ()
rrr = playDo _EMPTY_BOARD_

main :: IO ()
main =  firstPlayer >>= \sss -> rrr sss >> return ()

