addTuple :: Num a => (a,a) -> a
addTuple (x,y) = x + y

isZero :: Int -> Bool
isZero 0 = True
isZero x = False

lucky 3 = "Lucky"
lucky 7 = "So very lucky"
lucky 21 = "The luckiest"
lucky _  = "Not that lucky, to be honest."

isEmpty [] = True
isEmpty lst = False

startsWithSeven (7:xs) = True
startsWithSeven (x:xs) = False
startsWithSeven [] = False

tell :: (Show a) => [a] -> String
tell [] = "An empty list!"
tell (x:[]) = "The list has one element: " ++ (show x)
tell [x,y] = "The list has two elements: " ++ (show x) ++ ", " ++ (show y)
tell (x:y:xs) = "Long list! The first two elements: " ++ (show x) ++ ", " ++ (show y)

endsWithSeven lst = startsWithSeven (reverse lst)

{- tragically doesn't work
even (2*x) = True
even _ = False
-}

drop5 [] = []
drop5 lst@(x:xs) = if x == 5
                   then xs
                   else lst

firstLetter :: String -> String
--firstLetter str@(x:xs) = "The first letter of " ++ str ++ " is " ++ [x] ++ "."
firstLetter str@(x:xs) = "The first letter of " ++ str ++ " is " ++ (x:".")

isEven x 
  | (x `mod` 2 == 0) = True
  | otherwise        = False

mileage miles gallons
  | mpg <= 10 = "Get a new car"
  | mpg <= 20 = "You're doing okay."
  | mpg <= 40 = "You eco-warrior, you."
  | otherwise = "Don't lie son, it's not a good look."
  where mpg = miles / gallons

lexCmp :: Ord a => [a] -> [a] -> String
lexCmp (x:xs) (y:ys) 
  | x < y = "The first!"
  | y < x = "The second!"
  | otherwise = "Pretty close"
lexCmp (x:xs) [] = "The first!"
lexCmp [] (y:ys) = "The second!"
lexCmp [] [] = "The same"



cylinder r h = 
  let topArea = pi * r^2
      sideArea = (2*pi*r)*h
  in 2*topArea + sideArea

cylinder r h = 2*topArea + sideArea
  where topArea = pi * r^2
        sideArea = (2*pi*r)*h

doubleSmallNumber x = if x < 10 
                      then let y = 2 * x 
                           in y + 3
                      else x
