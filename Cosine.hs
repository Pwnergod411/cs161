
-- Cosine.hs
-- 
-- Defines a function that takes in 3 arguements a,b, and gamma, and returns 
-- the length of the side c using the law of cosines.

   degree y =  y * ( pi/180 )
   square x = x * x
   law_of_cosines a b gamma = sqrt (square a + square b -( 2*a*b*cos(degree gamma)))
