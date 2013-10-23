-- Hypotenuse.hs
--
-- Define a function for computing the length of the hypotenuse
-- of a right triangle, using the pythagorian theorem.

square x = x * x
hyp x y = sqrt ( square x + square y )
