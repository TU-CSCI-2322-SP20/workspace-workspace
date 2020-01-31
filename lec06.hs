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
--prod [7,4,1,2] = 7 * 3 * 1 * 5

prodEven :: [Integer] -> Integer
--prodEven [7,4,1,2] = 4 * 2
