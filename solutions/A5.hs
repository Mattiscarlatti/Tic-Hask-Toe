module A5 where

import A1
import A2
import A3
import A4

--import System.Random.Stateful (globalStdGen, uniformM)
import Control.Monad (when)

-- *** Assignment 5-1 *** --

-- Q#01
printBoard :: Board -> IO ()
printBoard iii = putStrLn $ formatBoard iii 

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./logo.txt"

printLogo :: IO ()
printLogo = readFile _LOGO_PATH_ >>= \s -> putStrLn s

-- Q#03
_RANDOM_BOOL_ :: IO Bool
--_RANDOM_BOOL_ = uniformM globalStdGen
_RANDOM_BOOL_ = return(True)

firstPlayer :: IO Player
firstPlayer = _RANDOM_BOOL_ >>= \jjj -> return . getFirstPlayer $ jjj

-- Q#04
getMove ::  Board -> IO (Int, Int)
getMove lll = getLine >>= \kkk -> case (isValidMove lll (stringToMove kkk)) of 
    True -> return (stringToMove kkk)
    False -> putStrLn "The value you printed is not valid. Please try again!" >> getMove lll

-- Q#05
play :: Board -> Player -> IO ()
play mmm nnn = case _DISPLAY_LOGO_ of
    True -> printLogo >> printBoard mmm >> return(promptPlayer nnn) >>= \ooo -> putStrLn ooo >> getMove mmm >>= \ppp -> return (playMove nnn mmm ppp) >>= \qqq -> case qqq of
        (InProgress, _) -> play (snd(qqq)) (switchPlayer nnn) >> return ()
        (Tie, _) -> printBoard (snd(qqq)) >> print (showGameState Tie) >> return ()
        (Xwon, _) -> printBoard (snd(qqq)) >> print (showGameState Xwon) >> return ()
        (Owon, _) -> printBoard (snd(qqq)) >> print (showGameState Owon) >> return ()
    False -> return ()

-- *** Assignment 5-2 *** --

-- Q#07
printLogoDo :: IO ()
printLogoDo = do
    ttt <- readFile _LOGO_PATH_
    putStrLn ttt

-- Q#08
firstPlayerDo :: IO Player
firstPlayerDo = do 
    uuu <- _RANDOM_BOOL_ 
    return . getFirstPlayer $ uuu

-- Q#09
getMoveDo :: Board -> IO (Int, Int)
getMoveDo vvv = do
     wwww <- getLine
     case (isValidMove vvv (stringToMove wwww)) of 
         True -> return (stringToMove wwww)
         False -> putStrLn "The value you printed is not valid. Please try again!" >> getMoveDo vvv

-- Q#10
playDo :: Board -> Player -> IO ()
playDo xxxx yyyy = do
         printLogo
         printBoard xxxx
         zzz <- return(promptPlayer yyyy) 
         putStrLn zzz
         zzzz <- getMove xxxx
         zzzzz <- return (playMove yyyy xxxx zzzz) 
         case zzzzz of
             (InProgress, _) -> play (snd(zzzzz)) (switchPlayer yyyy) >> return ()
             (Tie, _) -> printBoard (snd(zzzzz)) >> print (showGameState Tie) >> return ()
             (Xwon, _) -> printBoard (snd(zzzzz)) >> print (showGameState Xwon) >> return ()
             (Owon, _) -> printBoard (snd(zzzzz)) >> print (showGameState Owon) >> return ()