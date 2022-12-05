main :: IO()
main = do
    f <- readFile "Day4.txt"
    let list = lines f 
    let input = map getNumbers list

    let n1 = nOfContains input
    print ("Part1: " ++ show n1)
    
    let n2 = nOfOverlaps input
    print ("Part2: " ++ show n2)


-- Deriving the numbers --
getNumbers :: String -> ([Integer], [Integer])
getNumbers str = ([(toInteger' s1)..(toInteger' f1)], [(toInteger' s2)..(toInteger' f2)])
    where (left,',':right) = break (',' ==) str
          (s1,'-':f1) = break ('-' ==) left
          (s2,'-':f2) = break ('-' ==) right

toInteger' :: String -> Integer
toInteger' str = read str :: Integer


-- Part 1 --
nOfContains :: [([Integer], [Integer])] -> Int
nOfContains = length. filter containsAnother

-- Check if either of two lists contains the other
containsAnother :: ([Integer], [Integer]) -> Bool
containsAnother (l1,l2) = oneInTwo || twoInOne
   where oneInTwo = isSubList l1 l2
         twoInOne = isSubList l2 l1
         
-- Determines if the second list is a sublist of the first one
isSubList :: [Integer] -> [Integer] -> Bool
isSubList l1 = all (\i -> elem i l1) 



-- Part2 --
nOfOverlaps :: [([Integer], [Integer])] -> Int
nOfOverlaps = length. filter hasOverlap

hasOverlap :: ([Integer], [Integer]) -> Bool
hasOverlap (l1,l2) = oneInTwo || twoInOne
   where oneInTwo = overlaps l1 l2
         twoInOne = overlaps l2 l1

overlaps :: [Integer] -> [Integer] -> Bool
overlaps l1 = any (\i -> elem i l1) 


