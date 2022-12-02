import Data.List(groupBy, sort)

main :: IO ()
main = do
    f <- readFile "Day1.txt"
    let list = map tail $ groupBy (\_ s -> s/="") $ lines f
    let sums = map (\row -> sum $ map (\s -> read s :: Integer) row) list
    let maxSum = last $ sort sums
    let topThree = sum $ take 3 $ reverse $ sort sums
    putStrLn(show sums)
    putStrLn(show maxSum)
    putStrLn(show topThree)