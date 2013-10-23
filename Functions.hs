

as :: [Int]
as = [1..10]

bs = map (^2) as


data Complex = Complex Double Double
    deriving (Show,Eq)

instance Num Complex where
    Complex a b + Complex c d = Complex (a+c) (b+d)
    Complex a b * Complex c d = Complex (a*c-b*d) (a*d+b*c)
    abs (Complex a b) = Complex (sqrt (a**2+b**2)) 0
    signum (Complex a b) = Complex (a/sqrt(a**2+b**2)) (b/sqrt(a**2+b**2))
    fromInteger a = Complex (fromIntegral a) 0


