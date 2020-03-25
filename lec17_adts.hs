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




