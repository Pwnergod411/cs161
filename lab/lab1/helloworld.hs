module Main where
-- The main function is the "entry point" to your program
main = do
    putStrLn "What is the your name?" -- Write text to terminal
    inputName <- getLine -- Get something from user
    putStrLn (greet inputName) -- Greet the user
    

greet name = "Hello " ++ name ++ "!"
