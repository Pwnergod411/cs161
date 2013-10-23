-- NaturalNumbers.hs
    data NaturalNumbers = Zero | S NaturalNumber
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
	Zero == S _ = false
	S _ == Zero = false
	S x == S y = x == y

    instance Ord NaturalNumber where
	compare Zero Zero   = EQ
	compare x S x = LT
	compare S x x = GT

