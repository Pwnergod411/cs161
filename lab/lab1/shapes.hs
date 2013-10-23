
module Main where

-- ===================================
-- Logic to implement menu system.
-- Ignore this section!  Look down the bottom of the file
-- ===================================
main = menu

menu = do
    putStr $ unlines $ map  numText choices
    choice <- getLine
    case (getFun (read choice)) of
        Just f -> f
        Nothing -> putStrLn "Invalid choice, try again"
    menu
    where numText (n, t, fn) = (show n) ++ ") " ++ t

choices :: [(Int, String, IO())]
choices = [(1, "Area of square", squareArea),
            (2, "Area of triangle", triangleArea),
            (3, "Volume of cylinder", cylinderVolume)]

getFun :: Int -> Maybe (IO ())
getFun i = if length matches > 0 
            then Just f
            else Nothing
    where 
        (_, _, f) = head matches
        matches = filter (\(j, _, _) -> i == j) choices


squareArea = do
                 putStrLn "Length of square side:"
                 x <- getLine
                 putStrLn $ "Area: " ++ (show $ areaOfSquare (read x))

triangleArea = do
                 putStrLn "Triangle height:"
                 h <- getLine
                 putStrLn "Triangle base:"
                 b <- getLine
                 putStrLn $ "Area: " ++ 
                                (show $ areaOfTriangle (read b) (read h))
cylinderVolume = do
                 putStrLn "Cylinder height:"
                 h <- getLine
                 putStrLn "Cylinder radius:"
                 r <- getLine
                 putStrLn $ "Volume: " ++ 
                                (show $ volumeOfCylinder (read r) (read h))

-- ===================================
-- Fill in functions below.
-- Type definitions have been set up for you - you will need to define the
-- functions
-- ===================================

-- areaOfSquare takes one argument: length of side
areaOfSquare :: Num a => a -> a
areaOfSquare a = a^2
-- areaOfTriangle taskes two arguments: length of base, and triangle height
areaOfTriangle :: Fractional a => a -> a -> a
areaOfTriangle base height = (1/2 * base) * height
-- volumeOfCylinder takes two arguments; radius and height
volumeOfCylinder :: Floating a => a -> a -> a
volumeOfCylinder radius height = pi * radius^2 * height
