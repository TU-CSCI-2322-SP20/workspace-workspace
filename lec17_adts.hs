-- data TypeName = ConstructorName [FieldType .. ]| ConstructorName [FieldType ..]

--data Bool = True | False
data Contest = Rock | Scissors | Paper deriving (Show, Eq)

showSmaller :: (Ord a, Show a) => a -> a -> String
showSmaller x y = if x < y then show x else show y

shortHand :: Contest -> Char
shortHand Rock = 'R'
shortHand Scissors = 'S'
shortHand Paper = 'P'

rockPaperScissors :: Contest -> Contest -> String
rockPaperScissors Rock Scissors = "Player One Wins!"
rockPaperScissors Paper Rock  = "Player One Wins!"
rockPaperScissors Scissors Paper = "Player One Wins!"
rockPaperScissors Scissors Rock = "Player Two Wins!"
rockPaperScissors Rock Paper = "Player Two Wins!"
rockPaperScissors Paper Scissors = "Player Two Wins!"
rockPaperScissors _ _ = "It's a tie!"

data Velocity = FeetPerSecond Double | MetersPerSecond Double deriving Eq

readAsMPS :: Velocity -> Double
readAsMPS (FeetPerSecond x) = x * 0.3048
readAsMPS (MetersPerSecond x) = x

--A circle is a radius, an x coordinate and a y coordinate for the center
--A rectangle is the lower-left and upper-right  points, stored as x0 y0 x1 y1
type Point = (Double, Double)
data Shape = Circle Double Point | Rectangle Point Point | Triangle Point Point Point deriving (Eq, Show)
--aaah, area of a triangle is non-trivial.

area :: Shape -> Double
area (Circle r center) = pi * r^2
area (Rectangle (x0, y0) (x1, y1)) = abs ((x1-x0)*(y1-y0))


perimeter :: Shape -> Double
perimeter (Circle r center) = 2 * pi * r
perimeter (Rectangle (x0, y0) (x1, y1)) = abs $ (x1-x0)*2+(y1-y0)*2

square :: Point -> Double -> Shape
square (x,y) l = Rectangle (x,y) (x+l, y+l)

isSquare :: Shape -> Bool
isSquare (Circle _ _) = False
isSquare (Rectangle (x0,y0) (x1,y1)) = (abs (x1-x0)) == (abs (y1-y0))
