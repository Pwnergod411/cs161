import Prelude hiding (sum,product)
productf f [] = 1
productf f (c:cs) = f c * productf f cs

square x = x^2

product_sq xs = productf square xs

