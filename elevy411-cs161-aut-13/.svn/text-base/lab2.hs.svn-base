-- A point is a point in the xy plane, represented by x and y coordinates
-- E.g. (Point 0.0 0.0) is the origin, (Point (-1) (1)) is in the top left
-- quadrant.
data Point = Point Double Double
    deriving (Show, Eq)

getY (Point x y) = y
getX (Point x y) = x
 

-- A line segment is a straight line of finite length, defined by its
-- two end points.   E.g. (LineSegment (Point 0 0) (Point 1 1)) is a
-- line segment from the origin to the coordinate (1, 1)
data LineSegment = LineSegment Point Point 
    deriving (Show, Eq)
-- Distance Formula    
segmentLength (LineSegment a b) = sqrt((getX(b)-getX(a))**2 + (getY(b)-getY(a))**2)
-- Slope Formula
segmentSlope (LineSegment p1 p2) = (getY(p2) - getY(p1))/(getX(p2)-getX(p1))



-- A Path is a 2D path in the xy-plane.  The idea is that Path can be 
-- extended to support straight lines, curves, and arbitrary paths, 
-- but currently there is only one data constructor for Path: Line.
data Path = 
-- Line represents an infinite straight line defined by its slope a
-- and its y intercept b, ie. by the equation y = ax + b
    Line Double Double
    deriving (Show, Eq)

getSlope (Line m b) = m
getIntercept (Line m b) = b

intersects (Line m b ) (LineSegment (Point x1 y1) (Point x2 y2)) -- This Function determines if a path(line) and a segment intersect. 
    | m == segmentSlope(p1p2) = False
    | m*x1+b <= y1 && m*x2+b >= y2 = True
    | m*x1+b >= y1 && m*x2+b <= y2 = True
    | otherwise = False
    where 
        p1p2 = LineSegment (Point x1 y1) (Point x2 y2)

data Shape = Triangle Point Point Point -- This defines the shape either as a triangle, quadrilateral or a circle based on either 3 points, 4 points or the center point and radius. 
    | Quadrilateral Point Point Point Point
    | Circle Point Double
    deriving (Show,Eq)
  
data BoundingBox = BoundingBox Point Point -- The points are the Bottom Left and Top Right corners.
    deriving (Show,Eq)

-- boundShape simply takes the smallest x values and the smallest y values, as well as the largest x and largest y values in order to get the box, which corners are at the smallest xy pair and the largest xy pair.
boundShape (Triangle (Point x1 y1)(Point x2 y2)(Point x3 y3)) =  BoundingBox (Point smallX smallY)(Point bigX bigY) where
    smallX = minimum [x1,x2,x3]
    smallY = minimum [y1,y2,y3]
    bigX = maximum [x1,x2,x3]
    bigY = maximum [y1,y2,y3]

boundShape (Quadrilateral (Point x1 y1) (Point x2 y2) (Point x3 y3) (Point x4 y4)) =  BoundingBox (Point smallX smallY) (Point bigX bigY) where
     smallX = minimum [x1,x2,x3,x4]
     smallY = minimum [y1,y2,y3,y4]
     bigX = maximum [x1,x2,x3,x4]
     bigY = maximum [y1,y2,y3,y4]

boundShape (Circle (Point centerX centerY)  radius) = BoundingBox (Point smallX smallY) (Point bigX bigY) where
    smallX = centerX - radius
    smallY = centerY - radius
    bigX = centerX + radius
    bigY = centerY + radius
-- intersectsBB takes most of its code from intersects.
intersectsBB (BoundingBox (Point smallX smallY) (Point bigX bigY)) (Line m b) 
    | m*smallX+b <= smallY &&  m*bigX+b >= smallY = True
    | m*smallX+b >= smallY &&  m*bigX+b <= bigY   = True
    | otherwise = False
-- mightIntersectShape just checks if the boundingbox of the shape is intersected by the line and if so, returns true. Otherwise it returns false. 
mightIntersectShape (Circle center radius) (Line m b) 
    | intersectsBB (boundShape (Circle center radius)) (Line m b) == True = True
    | otherwise = False
    
mightIntersectShape (Quadrilateral p1 p2 p3 p4) (Line m b) 
    | intersectsBB (boundShape (Quadrilateral p1 p2 p3 p4)) (Line m b) == True = True
    | otherwise = False

mightIntersectShape (Triangle p1 p2 p3) (Line m b) 
    | intersectsBB (boundShape (Triangle p1 p2 p3)) (Line m b) == True = True
    | otherwise = False

