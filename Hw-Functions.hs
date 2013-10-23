--Exercise 6.1

(^2) :: Num a => a -> a

map :: (a -> b) -> [a] -> [b]

--Exercise 6.2

data Complex = Complex Double Double
    deriving (Show,Eq)
    
instance Num Complex where
    Complex a b + Complex c d = Complex (a+c) (b+d)
    Complex a b * Complex c d = Complex (a*c-b*d) (a*d+b*c)
    abs (Complex a b) = Complex (sqrt((a*a)+(b*b)) 0)
    signum Complex a b = Complex a/sqrt((a*a) + (b*b)) b/sqrt((a*a) + (b*b)) 
    fromInteger a = Complex a 0

