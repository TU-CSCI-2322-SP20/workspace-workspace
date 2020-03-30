myMaximum :: Ord a => [a] -> a
myMaximum [] = error "AAAAAAA"
myMaximum [x] = x
myMaximum (x:xs) = max x (myMaximum xs)

myLength :: [a] -> Integer
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

mySum :: [Integer] -> Integer
mySum [] = 0
mySum (x:xs) = x + mySum xs

myUnwords :: [String] -> String
myUnwords [] = ""
myUnwords (s:ss) = s ++ " " ++ (myUnwords ss)

--katamari :: (a -> a -> a) -> a -> [a] -> a
katamari :: (a -> b -> b) -> b -> [a] -> b
katamari f b [] = b
katamari f b (s:ss) = f s (katamari f b ss)

--iramatak :: (a -> a -> a) -> a -> [a] -> a
iramatak :: (b -> a -> b) -> b -> [a] -> b
iramatak f b [] = b
iramatak f b (s:ss) = iramatak f (f b s) ss

score :: [Char] -> Int
score "" = 0
score (c:cs) = (scoreLetter c) + (score cs)

scoreLetter :: Char -> Int
scoreLetter c
          | c `elem` "EAIONRTLSU" = 1
          | c `elem` "DG" = 2
          | c `elem` "BCMP" = 3
          | c `elem` "FHVWY" = 4
          | c == 'K' = 5
          | c `elem` "JX" = 8
          | c `elem` "QZ" = 10
          | otherwise = error "AAAAH"

