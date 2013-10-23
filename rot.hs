module Main where

import Data.Char
import System.Environment
import System.Exit
import Text.Regex.Posix

rot :: Int ->  String -> String


rot = map rot_char where
    rot_char c
        | isLower c = rot_case 'a' c
        | isUpper c = rot_case 'A' c
        | otherwise = c
    rot_case cas x char = chr ( ord cas + (ord char - ord cas + x) `mod` 26)

rot_stdin :: Int -> IO ()
rot_stdin n = do
    input <- getContents
    let output = rot 13 input
    putStr output

usage :: IO ()
usage = do
    progname <- getProgName
    hPutStrLn stderr $ "usage: " ++ progname ++ " [n]"
    exitWith $  ExitFailure 255                             

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> rot_stdin 13
        [x]
            | x =~ "^-?[0-9]+$" -> rot_stdin (read x)
            | otherwise -> usage
             

