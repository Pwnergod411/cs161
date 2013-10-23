
collatz :: Int -> Int
collatz x | odd x = (3*x)+1
          | otherwise = (x `div` 2)

takeUntil _ [] = []
takeUntil p (x:xs)
    | p x   = x : takeUntil p xs
    | otherwise = [1]

cseq x = (takeUntil (/= 1) $ iterate collatz x)
