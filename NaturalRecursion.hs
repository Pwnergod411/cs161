-- NaturalRecursion.hs
import Prelude hiding (foldr,sum,product,filter,map,(+),(*))
foldr combiner base [] = base
foldr combiner base (x:xs) = combiner x (foldr combiner base xs)

--sum = foldr (+) 0
--filter p = foldr p' [] where
  --  p' x xs
    --    | p x       = x : xs
      --  | otherwise = xs


--product = foldr (*) 1

--map :: (a->b) -> [a] -> [b]
--map f xs = foldr g [] xs where --Learn You A Haskell
 --   g x ys = f x : ys

data NaturalNumber = Zero | S NaturalNumber
    deriving (Show)

zero = Zero
one = S zero
two = S one
three = S two
four = S three
five = S four
six = S five
seven = S six
eight = S seven
nine = S eight
ten = S nine
infinity = S infinity
instance Eq NaturalNumber where
    Zero == Zero = True
    Zero == S _ = False
    S _ == Zero = False
    S x == S y = x == y

primrec f a Zero = a
primrec f a (S n) = f n $ primrec f a n

x + y = primrec f x y
    where f _ m = S m
    
x * Zero = Zero
x * (S y) = primrec (\_ m -> x + m) x y 


    

