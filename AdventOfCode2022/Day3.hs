import Data.Map as M (fromList, Map, lookup)
import Data.Maybe(fromJust)


main :: IO()
main = do
    f <- readFile "Day3.txt"
    let list = lines f
    let finalPoints = getAllPoints list
    --print list
    print finalPoints
    let listPart2 = partition list
    let finalPointsPart2 = getAllPoints' listPart2
    print listPart2
    print finalPointsPart2


-- Part 1 --
getAllPoints :: [String] -> Integer
getAllPoints = sum . map (getPoints . findDup)

findDup :: String -> Char
findDup str = head $ filter (\c-> elem c right) left
  where (left, right) = splitAt (div (length str) 2) str

getPoints :: Char -> Integer
getPoints c = fromJust $ M.lookup c points

points :: Map Char Integer
points = M.fromList $ zip "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" [1..52]



-- Part 2 --
partition :: [String] -> [[String]]
partition [] = []
partition (x:y:z:w) = [x,y,z] : partition w

findDup' :: [String] -> Char
findDup' [x,y,z] = head $ filter (\c -> elem c y && elem c z) x

getAllPoints' :: [[String]] -> Integer
getAllPoints' = sum . map (getPoints. findDup') 