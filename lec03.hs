--type String = [Char]


x = 3
aTuple :: (String, Integer)
aTuple = ("hello", 3)
aList = [7,3,1,5]

tupA = (7,3)
tupB = (6.5, 3.5)

--addPair :: (Int, Int) -> Int
--addPair tup = (fst tup) + (snd tup)
addPair (x,y) = x + y

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

tripA = (7,3,1)
addTriple (x,y,z) = x + y + z 

crossProduct alst blst = [(x,y) | x <- alst, y <- blst]

grades = [("Jane", 97), ("Carlo", 90), ("Davide", 50)]
