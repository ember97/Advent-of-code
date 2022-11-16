module Forest where
import Data.Matrix 

-- ** Problem description at https://adventofcode.com/2018/day/18 **

data Acre = Empty Bool | Tree Bool | Lumberyard Bool
    deriving (Eq)


instance Show Acre where
    show (Empty _) = "."
    show (Tree _)  = "|"
    show (Lumberyard _) = "#"


type Forest = Matrix Acre


-- Some helper functions
isTree :: Acre -> Bool
isTree (Tree _) = True
isTree _        = False

isLumberyard :: Acre -> Bool
isLumberyard (Lumberyard _) = True
isLumberyard _              = False

getStatus :: Acre -> Bool
getStatus (Empty b) = b
getStatus (Tree b) = b
getStatus (Lumberyard b) = b

changeStatus :: Acre -> Acre
changeStatus (Empty b)      = (Empty (not b))
changeStatus (Tree b)       = (Tree (not b))
changeStatus (Lumberyard b) = (Lumberyard (not b))

setFalse :: Acre -> Acre
setFalse (Empty b)      = (Empty False)
setFalse (Tree b)       = (Tree False)
setFalse (Lumberyard b) = (Lumberyard False)

createAcre :: Char -> Acre
createAcre '.' = (Empty False)
createAcre '|' = (Tree False)
createAcre '#' = (Lumberyard False)
createAcre _   = error ("Invalid character")


-- Returns all neighbors (including yourself!!!!)
getArea :: Int -> Int -> Matrix a -> Matrix a
getArea r c m
  | r == 1 && c == 1                 = submatrix 1 2 1 2 m
  | r == (nrows m) && c == (ncols m) = submatrix (r-1) r (c-1) c m
  | r == (nrows m) && c == 1         = submatrix (r-1) r c (c+1) m
  | r == 1         && c == (ncols m) = submatrix r (r+1) (c-1) c m 
  | r == 1                           = submatrix 1 2 (c-1) (c+1) m
  | c == 1                           = submatrix (r-1) (r+1) 1 2 m
  | r == (nrows m)                   = submatrix (r-1) r (c-1) (c+1) m
  | c == (ncols m)                   = submatrix (r-1) (r+1) (c-1) c m
  | otherwise                        = submatrix (r-1) (r+1) (c-1) (c+1) m


-- Check if we should evolve an acre
checkGrow :: Int -> Int -> Forest -> Bool
checkGrow r c f = let a = getElem r c f in case a of
    (Empty False)      -> nTrees >= 3 
    (Tree False)       -> nLumberyards >= 3 
    (Lumberyard False) -> not $ nLumberyards >= 2 && nTrees >= 1 
    _                  -> error ("Already grown")
  where (nTrees, nLumberyards) = count r c f


-- Returns the number of trees and lumberyards in the area surrounding a specific element
count :: Int -> Int -> Forest -> (Int, Int)
count r c f = (nTrees, nLumberyards)
    where area = concat $ toLists $ getArea r c f -- Will be small so compl prob ok
          nTrees = length $ filter isTree area
          nLumberyards = length $ filter isLumberyard area

-- Evolve acres that we have checked!
grow :: Acre -> Acre
grow (Empty True)      = (Tree False)
grow (Tree True)       = (Lumberyard False)
grow (Lumberyard True) = (Empty False)
grow a                 = a

-- Loops through a forest and check if each element will grow in the next step
checkGrowAll :: Forest -> Forest
checkGrowAll = go 1 1 
  where go :: Int -> Int -> Forest -> Forest
        go i j f
           | endLoop = newF
           | newIter = go (i+1) 1 newF
           | otherwise = go i (j+1) newF
         where endLoop = (i == nrows f) && (j == ncols f)
               newIter = (j == ncols f)
               willGrow = checkGrow i j f
               newF = if willGrow then setElem (changeStatus (unsafeGet i j f)) (i,j) f else f

-- Get the resource value of a forest
resourceValue :: Forest -> (Int, Int)
resourceValue f = (nTrees, nLumberyards)
    where list = concat $ toLists f
          nTrees = length $ filter isTree list
          nLumberyards = length $ filter isLumberyard list

-- Perform one iteration of the algorithm
iter :: Forest -> Forest
iter f = fmap grow checkedF
    where checkedF = checkGrowAll f 

-- Perform n iterations of the algorithm
loop :: Int -> Forest -> Forest
loop 0 f = f
loop n f = loop (n-1) (iter f)


main :: IO()
main = do
    f <- readFile "Day18.txt"
    let forest = fromLists $ map (\line -> map (\c-> createAcre c) line) (lines f)
    putStrLn(show forest)
    let iter1 = loop 10 forest
    putStrLn(show iter1)
    putStrLn(show (resourceValue iter1))