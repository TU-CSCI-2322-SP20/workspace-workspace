import Debug.Trace
--quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (pivot:xs) = 
    let bigger = [x | x <- xs, x >= pivot]
        smaller = [x | x <- xs, x < pivot]
    in (quickSort smaller) ++ [pivot] ++ (quickSort bigger)

hanoi 0 s m e = []
hanoi n s m e =
    let firstSteps = hanoi (n-1) s e m
        bigStep = (replicate n ' ') ++ "move " ++ show n ++ " from " ++ s ++ " to " ++ e ++ "."
        lastSteps = hanoi (n-1) m s e 
    in firstSteps ++ [bigStep] ++ lastSteps

countHanoi 0 = 0
countHanoi n = 2 * (countHanoi (n-1)) + 1
