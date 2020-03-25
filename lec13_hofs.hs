addFiveAll :: [Int] -> [Int]
addFiveAll [] = []
addFiveAll (x:xs) = (x+5):(addFiveAll xs)

absAll :: [Int] -> [Int]
absAll [] = []
absAll (x:xs) = (abs x):(absAll xs)

singletons :: [a] -> [[a]]
singletons [] = []
singletons (x:xs) = [x]:(singletons xs)

applyToAll :: (a -> b) -> [a] -> [b]
applyToAll f [] = []
applyToAll f (x:xs) = (f x):(applyToAll f xs)

addThree :: Int -> (Int -> (Int -> Int))
addThree x y z = x + y + z


sumAll :: [[Int]] -> [Int]
sumAll lst = map sum lst

tripleList :: [a] -> [a]
tripleList lst = concat $ map (replicate 3) lst

subThree :: [Int] -> [Int]
subThree lst = map (\x -> x -3) lst

--use map to define:
lengths :: [[a]] -> [Int]
lengths lst = map length lst

singletons2 :: [a] -> [[a]]
singletons2 lst = map (\x -> [x]) lst

shoutNumbers :: [Int] -> [String]
shoutNumbers xs = map (++"!") (map show xs)
shoutNumbers xs = 
  let shown = map show xs
  in map (++"!") shown
shoutNumbers xs = map (\x -> show x ++ "!") xs 
shoutNumbers xs = map ((++ "!") . show) xs 

uppers :: String -> String
uppers "" = ""
uppers (c:cs) = if 'A' < c && c < 'Z' 
                then c:(uppers cs)
                else uppers cs

evens :: [Int] -> [Int]
evens [] = []
evens (x:xs) = if x `mod` 2 == 0
               then x:(evens xs)
               else evens xs

positives :: [Int] -> [Int]
positives [] = []
positives (x:xs) = if x > 0 
                   then x:(positives xs)
                   else positives xs

keepOnly f [] = []
keepOnly f (x:xs) = if f x
                    then x:(keepOnly f xs)
                    else keepOnly f xs








