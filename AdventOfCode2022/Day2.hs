data Element = Rock | Paper | Scissors
   deriving (Show, Eq)

data Outcome = Lose | Win | Draw

main :: IO()
main = do
    f <- readFile "Day2.txt"
    let list = map (\[x,_,y] -> (transform x, transform y)) $ lines f
    let totalScore = sum $ map playRound list
    --putStrLn(show list)
    putStrLn(show totalScore)

    let list2 = map updateMe list
    let totalScore2 = sum $ map playRound list2
    --putStrLn(show list2)
    putStrLn(show totalScore2)

--- Part 1 ---
playRound :: (Element,Element) -> Integer
playRound (cpu, me) = (getPointsOutcome cpu me) + getPointsChoice me

transform :: Char -> Element
transform c 
   | elem c "AX" = Rock
   | elem c "BY" = Paper
   | otherwise   = Scissors

-- me cpu
getOutcome :: Element -> Element -> Outcome
getOutcome cpu me = if cpu == me then Draw else case (cpu, me) of
   (Rock, Scissors) -> Lose
   (Paper, Rock) -> Lose
   (Scissors, Paper) -> Lose
   _ -> Win

getPointsOutcome :: Element -> Element -> Integer
getPointsOutcome cpu me = let o = getOutcome cpu me in case o of
    Win -> 6
    Draw -> 3
    Lose -> 0

getPointsChoice :: Element -> Integer
getPointsChoice e = case e of 
    Rock -> 1
    Paper -> 2
    Scissors -> 3



--- part 2 ---
predictOutcome :: Element -> Outcome
predictOutcome me = case me of
    Rock -> Lose
    Paper -> Draw
    Scissors -> Win

toWin :: Element -> Element
toWin cpu = case cpu of 
    Rock -> Paper
    Paper -> Scissors
    Scissors -> Rock

toLose :: Element -> Element
toLose cpu = case cpu of
    Rock -> Scissors
    Paper -> Rock
    Scissors -> Paper

updateMe :: (Element, Element) -> (Element, Element)
updateMe (cpu, me) = let pOutcome = predictOutcome me in case pOutcome of
    Win -> (cpu, toWin cpu)
    Lose -> (cpu, toLose cpu)
    Draw -> (cpu, cpu)