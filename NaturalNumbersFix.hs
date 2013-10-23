-- NaturalNumber.hs
data NaturalNumber = Zero | S NaturalNumber
    deriving (Show)
-- common names for small natural numbers 
zero  = Zero
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven
nine  = S eight
ten   = S nine
infinity = S infinity
instance Eq NaturalNumber where
    Zero == Zero = True
    Zero == S _ = False
    S _ == Zero = False
    S x == S y = x == y

instance Ord NaturalNumber where
    compare Zero Zero   = EQ
    compare (S _) Zero = GT
    compare Zero (S _) = LT
    compare (S x) (S y) = compare x y

instance Num NaturalNumber where
    x + Zero = x
    x + S y = S (x + y)
    x * Zero = Zero
    x * S y = x + x * y
    x - Zero = x
    Zero - x = Zero
    S x - S y = x - y

    fromInteger n
        | n > 0 = S (fromInteger (n-1))
        | n == 0 = Zero
   
    signum n
        | n > Zero = one
        | n == Zero = 0 
       
    abs = id
nat :: NaturalNumber -> NaturalNumber
nat = id
       
