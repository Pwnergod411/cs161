data Branch x = Leaf x | Node (Branch x) x (Branch x)
    deriving (Show)

instance Functor Branch where
    fmap f(Leaf x) = Leaf (f x)
    fmap f(Node x y z) = Node (fmap f(x)) (f y) (fmap f(z))   
