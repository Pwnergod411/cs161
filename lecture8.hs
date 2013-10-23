
module Main where

import System.Environment
import Data.Char

data Poly = Poly [Double]


eval :: Poly -> Double -> Double

eval (Poly []) _ = 0

eval (Poly (x:xs)) v = baseCase x (eval (Poly (xs)) v)  where
     baseCase x y = x + v * y




           
msg :: String -> String
msg user = "Hello, " ++ (toUpper (head user) : tail user) ++ "!"
                    
main :: IO ()
main = do
    user <- getEnv "USER"
    putStrLn $ msg user
    

