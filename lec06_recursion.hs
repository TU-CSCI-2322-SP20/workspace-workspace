import Debug.Trace
lstA = [7,3,1]
powerLstA = [ [7,3,1]
            , [7,3]
            , [7,1]
            , [3,1]
            , [7]
            , [3]
            , [1]
            , []
            ]
lstB = [1,2,3,4]

union xs ys = xs ++ ys
intersect xs ys = [x | x <- xs, x `elem` ys]
--intersect xs ys = [y | y <- ys, y `elem` xs]
setDiff xs ys = [x | x <- xs, not (x `elem` ys)]
crossProduct xs ys = [ (x,y) | x <- xs, y <- ys]


isZero [] = True
isZero (x:xs)= False

myLength [] = 0
myLength (x:xs) = 1 + myLength xs

numNonZero :: [Integer] -> Integer
numNonZero [] = 0
numNonZero (x:xs) = if x == 0
                    then numInXs
                    else 1 + numInXs
        where numInXs = numNonZero xs

numNonZeroB :: [Integer] -> Integer
numNonZeroB [] = 0
numNonZeroB (0:xs) = numNonZero xs
numNonZeroB (x:xs) = 1 + numNonZero xs

prod :: [Integer] -> Integer
prod [] = 1
prod (x:xs) = (prod xs) * x
--prod [7,4,1,2] = 7 * 3 * 1 * 5

prodEven :: [Integer] -> Integer
prodEven [] = 1
prodEven (x:xs) = if even x 
                  then  x * prodEven xs
                  else prodEven xs
--prodEven [7,4,1,2] = 4 * 2
--Do it a second time, using a list comprehension and your previous function.
--
occurancesOfHead :: Eq a => [a] -> Int
occurancesOfHead [] = 0 -- error "Head of an empty list"
occurancesOfHead (x:xs) = (occurs x xs) + 1
 where occurs :: Eq a => a -> [a] -> Int
       occurs n [] = 0
       occurs n (x:xs) = 
         let numInXs = occurs n xs
         in if n == x then numInXs +1 else numInXs

--occurancesOfHeadAlt lst = occurs (head lst) lst

myMaximum [] = error "You can't take the maximum of an empty list."
myMaximum [x] = trace ("Base case: " ++ (show x)) x
myMaximum (x:xs) = traceShow (x,xs) $
                   let maxXs = myMaximum xs
                   in if x > maxXs
                      then x
                      else maxXs

--[7,3,1,2,7] -> 2
--[7,7,7,7] -> 4
--
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]
