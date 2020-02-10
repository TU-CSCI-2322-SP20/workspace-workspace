import Debug.Trace
gap :: [Int] -> Int
gap lst = let (small, large) = range lst
          in large - small

--return the tuple of (smallest, largest) element in the list
range :: [Int] -> (Int, Int)
range [] = error "AAAAAAAHH WHYYYY"
range [x] = (x,x)
range (x:xs) | x > largeXs = (smallXs, x)
             | x < smallXs = (x, largeXs)
             | otherwise   = (smallXs, largeXs)
  where (smallXs, largeXs)= range xs

hoursToSchedule :: [(Int, Int)] -> Int
hoursToSchedule tasks = let (h,m) = addTimes tasks
                        in if m > 0 
                           then h+1 
                           else h

addTimes :: [(Int, Int)] -> (Int, Int)
addTimes [] = (0,0)
addTimes ((h,m):ts) = 
  let (hourTs, minTs) = addTimes ts
      mins = m + minTs
      hours = h + hourTs + if mins > 60 then 1 else 0
  in (hours, mins `mod` 60)

secondLargest :: Ord a => [a] -> a
secondLargest lst = fst (aux lst)
  where aux :: Ord a => [a] -> (a,a)
        aux [x,y] = if x > y then (y,x) else (x,y) -- (min x y, max x y)
        aux (x:xs) = let (slXs, lXs) = aux xs
                     in if x > lXs 
                        then (lXs, x)
                        else if x > slXs 
                             then (x, lXs) 
                             else (slXs, lXs)
